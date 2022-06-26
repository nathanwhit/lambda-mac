use std::mem;

use im::Vector;
use smol_str::SmolStr;

use crate::{ast, debruijn::DebruijnIndex};

pub type Ident = SmolStr;

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Abstraction(Ident, Box<Term>),
    Application(Box<Term>, Box<Term>),
    Variable(DebruijnIndex),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Expr(Term),
    Bind(Ident, Term),
}

#[derive(Clone)]
pub struct Statement {
    pub stmt: Stmt,
    pub context: NamingContext,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    Free,
    Global,
}

#[derive(Clone)]
pub struct Binding {
    name: Ident,
    #[allow(dead_code)]
    kind: BindingKind,
}

#[derive(Clone)]
pub struct NamingContext {
    local_bindings: Vector<Ident>,
    global_bindings: Vector<Binding>,
}

impl ast::Term {
    pub fn lower(self, ctx: &mut NamingContext) -> Term {
        ctx.lower_term(self)
    }
}

impl NamingContext {
    pub fn non_local(&self) -> Self {
        Self {
            local_bindings: Vector::new(),
            global_bindings: self.global_bindings.clone(),
        }
    }
    pub fn enter<T>(&mut self, func: impl FnOnce(&mut NamingContext) -> T) -> T {
        let fresh = self.local_bindings.clone();
        let local_bindings = mem::replace(&mut self.local_bindings, fresh);
        let ret = func(self);
        self.local_bindings = local_bindings;
        ret
    }

    pub fn new() -> Self {
        Self {
            local_bindings: Vector::new(),
            global_bindings: Vector::new(),
        }
    }

    pub fn name_of(&self, idx: DebruijnIndex) -> Option<Ident> {
        let idx = idx.depth() as usize;
        self.local_bindings
            .iter()
            .chain(self.global_bindings.iter().map(|b| &b.name))
            .nth(idx)
            .map(|s| s.clone())
    }

    pub fn index_of(&self, name: &str) -> Option<DebruijnIndex> {
        self.local_bindings
            .iter()
            .chain(self.global_bindings.iter().map(|b| &b.name))
            .enumerate()
            .find_map(|(idx, n)| n.eq(name).then(|| DebruijnIndex::new(idx as u32)))
    }

    pub fn add_local(&mut self, name: Ident) -> DebruijnIndex {
        self.local_bindings.push_front(name);
        DebruijnIndex::new(0)
    }

    pub fn add_global(&mut self, name: Ident) -> DebruijnIndex {
        self.global_bindings.push_back(Binding {
            name,
            kind: BindingKind::Global,
        });
        DebruijnIndex::new((self.local_bindings.len() + self.global_bindings.len() - 1) as u32)
    }

    pub fn add_free(&mut self, name: Ident) -> DebruijnIndex {
        self.global_bindings.push_back(Binding {
            name,
            kind: BindingKind::Free,
        });
        DebruijnIndex::new((self.local_bindings.len() + self.global_bindings.len() - 1) as u32)
    }

    pub fn get_or_add_free(&mut self, name: &str) -> DebruijnIndex {
        self.index_of(name)
            .unwrap_or_else(|| self.add_global(name.into()))
    }
}

impl NamingContext {
    pub fn lower_term(&mut self, term: ast::Term) -> Term {
        match term {
            ast::Term::Abstraction(arg, body) => self.enter(|ctx| {
                ctx.add_local(arg.clone());
                Term::Abstraction(arg, Box::new(ctx.lower_term(*body)))
            }),
            ast::Term::Application(a, b) => Term::Application(
                self.enter(|ctx| Box::new(ctx.lower_term(*a))),
                self.enter(|ctx| Box::new(ctx.lower_term(*b))),
            ),
            ast::Term::Variable(id) => Term::Variable(self.get_or_add_free(&id)),
        }
    }

    pub fn print_term(&mut self, term: &Term) -> String {
        match term {
            Term::Abstraction(arg, body) => self.enter(|ctx| {
                ctx.add_local(arg.clone());
                format!("(λ{arg}. {})", ctx.print_term(&*body)).into()
            }),
            Term::Application(a, b) => self
                .enter(|ctx| format!("({} {})", ctx.print_term(&*a), ctx.print_term(&*b)).into()),
            Term::Variable(idx) => self
                .name_of(*idx)
                .expect(&format!("unbound variable {idx:?}"))
                .into(),
        }
    }
}

impl NamingContext {
    pub fn lower_stmt(&mut self, stmt: ast::Stmt) -> Statement {
        let stmt = match stmt {
            ast::Stmt::Expr(e) => Stmt::Expr(self.lower_term(e)),
            ast::Stmt::Bind(id, val) => {
                self.add_global(id.clone());
                Stmt::Bind(id, self.lower_term(val))
            }
        };
        Statement {
            stmt,
            context: self.non_local(),
        }
    }

    pub fn lower_stmts(&mut self, stmts: impl IntoIterator<Item = ast::Stmt>) -> Vec<Statement> {
        stmts.into_iter().map(|s| self.lower_stmt(s)).collect()
    }

    pub fn print_stmt(&mut self, stmt: &Stmt) -> String {
        match stmt {
            Stmt::Expr(e) => self.print_term(e),
            Stmt::Bind(id, e) => format!("{id} = {}", self.print_term(e)),
        }
    }
}

impl Term {
    pub fn print(&self, context: &mut NamingContext) -> String {
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
            $crate::ir::Term::Variable($crate::debruijn::DebruijnIndex::new($idx))
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
                        let mut ctx = $crate::ir::NamingContext::new();
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
                        let mut ctx = $crate::ir::NamingContext::new();
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
        basic_var   :   term("x")                   => t!(0);
        nested_abs  :   term("λx. λy. x")           => t!(x -> t!(y -> t!(1)));
        app_abs     :   term("(λ x. x) (λ y. y)")   => t!(t!(x -> t!(0)) => t!(y -> t!(0)));
        free_var    :   term("λx. x y")             => t!(x -> t!(t!(0) => t!(1)));
        shadowing   :   term("λx. λy. λx. x y")     => t!(x -> t!(y -> t!(x -> t!(t!(0) => t!(1)))));

        thing       :   term("((λx. λy. x) y) z")   => t!(t!(t!(x -> t!(y -> t!(1))), t!(0)), t!(1))
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
