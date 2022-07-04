use std::mem;

use im::Vector;
use smol_str::SmolStr;

use crate::{ast, debruijn::DebruijnIndex};

pub type Ident = SmolStr;

pub type Path = SmolStr;

#[derive(Debug, Eq, Clone)]
pub enum Term {
    Abstraction(Ident, Box<Term>),
    Application(Box<Term>, Box<Term>),
    Variable(DebruijnIndex, usize),
    Let(Ident, Box<Term>, Box<Term>),
}

impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Abstraction(l0, l1), Self::Abstraction(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Application(l0, l1), Self::Application(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Variable(l0, _), Self::Variable(r0, _)) => l0 == r0,
            (Self::Let(l0, l1, l2), Self::Let(r0, r1, r2)) => l0 == r0 && l1 == r1 && l2 == r2,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Term),
    Bind(Ident, Term),
    Import(Path),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum BindingKind {
    Free,
    Global(Term),
    Local,
}

#[derive(Clone, Debug)]
pub struct Binding {
    name: Ident,
    kind: BindingKind,
}

impl Binding {
    pub fn local(name: Ident) -> Self {
        Self {
            name,
            kind: BindingKind::Local,
        }
    }
    pub fn global(name: Ident, value: Term) -> Self {
        Self {
            name,
            kind: BindingKind::Global(value),
        }
    }
    pub fn free(name: Ident) -> Self {
        Self {
            name,
            kind: BindingKind::Free,
        }
    }
}

#[derive(Clone, Debug)]
pub struct BindingContext {
    bindings: Vector<Binding>,
}

impl ast::Term {
    pub fn lower(self, ctx: &mut BindingContext) -> Term {
        ctx.lower_term(self)
    }
}

impl BindingContext {
    pub fn enter<T>(&mut self, func: impl FnOnce(&mut BindingContext) -> T) -> T {
        let fresh = self.bindings.clone();
        let saved_bindings = mem::replace(&mut self.bindings, fresh);
        let ret = func(self);
        self.bindings = saved_bindings;
        ret
    }

    pub fn new() -> Self {
        Self {
            bindings: Vector::new(),
        }
    }

    pub fn name_of(&self, idx: DebruijnIndex) -> Option<Ident> {
        let idx = idx.depth() as usize;
        self.bindings.get(idx).map(|b| b.name.clone())
    }

    pub fn index_of(&self, name: &str) -> Option<DebruijnIndex> {
        self.bindings
            .iter()
            .enumerate()
            .find_map(|(idx, n)| n.name.eq(name).then(|| DebruijnIndex::new(idx as u32)))
    }

    pub fn add_local(&mut self, name: Ident) -> DebruijnIndex {
        self.bindings.push_front(Binding::local(name));
        DebruijnIndex::INNERMOST
    }

    pub fn add_global(&mut self, name: Ident, value: Term) -> DebruijnIndex {
        self.bindings.push_front(Binding::global(name, value));
        DebruijnIndex::INNERMOST
    }

    pub fn add_free(&mut self, name: Ident) -> DebruijnIndex {
        self.bindings.push_front(Binding::free(name));
        DebruijnIndex::INNERMOST
    }

    pub fn get_global(&self, idx: DebruijnIndex) -> Option<Term> {
        match self.bindings.get(idx.depth() as usize) {
            Some(Binding {
                name: _,
                kind: BindingKind::Global(value),
            }) => Some(value.clone().shifted_in(idx.shifted_in())),
            _ => None,
        }
    }
}

impl BindingContext {
    #[tracing::instrument(skip(self))]
    pub fn lower_term(&mut self, term: ast::Term) -> Term {
        match term {
            ast::Term::Abstraction(arg, body) => self.enter(|ctx| {
                ctx.add_local(arg.clone());
                Term::Abstraction(arg, Box::new(ctx.lower_term(*body)))
            }),
            ast::Term::Application(a, b) => {
                Term::Application(Box::new(self.lower_term(*a)), Box::new(self.lower_term(*b)))
            }
            ast::Term::Variable(id) => Term::Variable(
                self.index_of(&*id).expect(&format!("unbound var {}", id)),
                self.bindings.len(),
            ),
            ast::Term::Let(id, value, body) => {
                let value = self.lower_term(*value);
                self.enter(|ctx| {
                    ctx.add_local(id.clone());
                    Term::Let(id, Box::new(value), Box::new(ctx.lower_term(*body)))
                })
            }
            ast::Term::MacroDef(..) => panic!("macro definitions should be erased before lowering"),
        }
    }

    #[tracing::instrument(skip(self))]
    pub fn print_term(&mut self, term: &Term) -> String {
        match term {
            Term::Abstraction(arg, body) => self.enter(|ctx| {
                ctx.add_local(arg.clone());
                format!("(λ{arg}. {})", ctx.print_term(&*body)).into()
            }),
            Term::Application(a, b) => self
                .enter(|ctx| format!("({} {})", ctx.print_term(&*a), ctx.print_term(&*b)).into()),
            Term::Variable(idx, len) => {
                assert_eq!(self.bindings.len(), *len);
                self.name_of(*idx)
                    .expect(&format!("unbound variable {idx:?}"))
                    .into()
            }
            Term::Let(id, value, body) => {
                let value_print = self.print_term(&*value);
                self.enter(|ctx| {
                    ctx.add_local(id.clone());

                    format!("let {id} = {value_print} in {}", ctx.print_term(&*body))
                })
            }
        }
    }
}

impl BindingContext {
    pub fn print_stmt(&mut self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Expr(e) => self.print_term(e),
            Stmt::Bind(id, e) => format!("{id} = {}", self.print_term(e)),
            Stmt::Import(path) => format!("import \"{path}\""),
        }
    }
}

impl Term {
    pub fn print(&self, context: &mut BindingContext) -> String {
        context.print_term(self)
    }

    pub fn is_value(&self) -> bool {
        matches!(self, Term::Abstraction(_, _))
    }

    pub fn body(&self) -> Option<&Term> {
        match self {
            Term::Abstraction(_, body) => Some(body),
            _ => None,
        }
    }
}

#[cfg(test)]
pub(crate) mod test {
    use crate::parse::term;

    macro_rules! assert_lowers {
        ($ctx:expr; $thing:expr, $expect:expr) => {
            ::pretty_assertions::assert_eq!($ctx.lower_term($thing.unwrap().1), $expect)
        };
        ($ctx:expr; $($thing: expr => $expect: expr),* $(,)?) => {
            $(
                assert_lowers!($ctx; $thing, $expect);
            )*
        };
    }

    macro_rules! assert_prints {
        ($ctx:expr; $thing: expr, $expect: expr) => {
            let lowered = $ctx.lower_term($thing.unwrap().1);
            ::pretty_assertions::assert_eq!(
                $ctx.print_term(
                    &lowered
                )
                .as_str(),
                $expect
            )
        };
        ($ctx:expr; $(thing: expr => $expect: expr),* $(,)?) => {
            $(
                assert_prints!(ctx; thing, $expect);
            )*
        };
    }

    macro_rules! t_ {
        ($ident: ident) => {
            ::smol_str::SmolStr::from(stringify!($ident))
        };
        ($idx: expr) => {
            $crate::ir::Term::Variable($crate::debruijn::DebruijnIndex::new($idx), 0)
        };
        ($v: ident -> $b: expr) => {
            $crate::ir::Term::Abstraction(
                ::smol_str::SmolStr::from(stringify!($v)),
                Box::new($b.into()),
            )
        };
        ($a: expr , $b: expr) => {
            $crate::ir::Term::Application(Box::new($a.into()), Box::new($b.into()))
        };
        ($a: expr => $b: expr) => {
            t!($a, $b)
        };
    }

    macro_rules! lowering_tests {
        ($($test: ident : $a: expr => $b: expr);* $(;)?) => {
            $(
                paste::paste! {
                    #[test]
                    fn [<test_lower_ $test>]() {
                        let mut ctx = $crate::ir::BindingContext::new();
                        ctx.add_free("x".into());
                        ctx.add_free("y".into());
                        ctx.add_free("z".into());
                        assert_lowers!(ctx; $a, $b);
                    }
                }
            )*
        };
    }

    macro_rules! printing_tests {
        ($($test: ident : $a: expr => $b: expr);* $(;)?) => {
            $(
                paste::paste! {
                    #[test]
                    fn [<test_print_ $test>]() {
                        let mut ctx = $crate::ir::BindingContext::new();
                        ctx.add_free("x".into());
                        ctx.add_free("y".into());
                        ctx.add_free("z".into());
                        assert_prints!(ctx; $a, $b);
                    }
                }
            )*
        };
    }

    pub(crate) use t_ as t;

    lowering_tests! {
        basic_abs   :   term("λx. x")               => t!(x -> t!(0));
        basic_app   :   term("λx. x x")             => t!(x -> t!(t!(0) => t!(0)));
        basic_var   :   term("x")                   => t!(2);
        nested_abs  :   term("λx. λy. x")           => t!(x -> t!(y -> t!(1)));
        app_abs     :   term("(λ x. x) (λ y. y)")   => t!(t!(x -> t!(0)) => t!(y -> t!(0)));
        free_var    :   term("λx. x y")             => t!(x -> t!(t!(0) => t!(2)));
        shadowing   :   term("λx. λy. λx. x y")     => t!(x -> t!(y -> t!(x -> t!(t!(0) => t!(1)))));

        thing       :   term("((λx. λy. x) y) z")   => t!(t!(t!(x -> t!(y -> t!(1))), t!(1)), t!(0));
    }

    printing_tests! {
        basic_abs   :   term("λx. x")               => "(λx. x)";
        basic_app   :   term("λx. x x")             => "(λx. (x x))";
        basic_var   :   term("x")                   => "x";
        nested_abs  :   term("λx. λy. x")           => "(λx. (λy. x))";
        app_abs     :   term("(λ x. x) (λ y. y)")   => "((λx. x) (λy. y))";
        free_var    :   term("λx. x y")             => "(λx. (x y))";
        shadowing   :   term("λx. λy. λx. x y")     => "(λx. (λy. (λx. (x y))))";
        thing       :   term("((λx. λy. x) y) z")   => "(((λx. (λy. x)) y) z)";
    }
}
