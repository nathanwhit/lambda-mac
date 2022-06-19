use im::{HashMap, Vector};

use crate::{ast, debruijn::DebruijnIndex};

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Abstraction(String, Box<Term>),
    Application(Box<Term>, Box<Term>),
    Variable(DebruijnIndex),
}

type LowerContext = Vector<String>;
type GlobalContext = HashMap<DebruijnIndex, String>;

impl ast::Term {
    fn lower_(self, context: LowerContext, global: &mut GlobalContext) -> Term {
        match self {
            ast::Term::Abstraction(arg, body) => {
                let mut context = context.clone();
                context.push_front(arg.clone());
                Term::Abstraction(arg, Box::new(body.lower_(context, global)))
            }
            ast::Term::Application(a, b) => Term::Application(
                Box::new(a.lower_(context.clone(), global)),
                Box::new(b.lower_(context, global)),
            ),
            ast::Term::Variable(id) => {
                Term::Variable(DebruijnIndex::new(match context.index_of(&id) {
                    Some(idx) => idx as u32,
                    None => {
                        let idx = (global.len() + context.len()) as u32;
                        global.insert(DebruijnIndex::new(idx), id.clone());
                        idx
                    }
                }))
            }
        }
    }

    pub fn lower(self) -> (Term, GlobalContext) {
        let mut global = HashMap::new();
        (self.lower_(Vector::new(), &mut global), global)
    }
}

impl Term {
    pub fn print(&self, context: LowerContext, global: &GlobalContext) -> String {
        match self {
            Term::Abstraction(arg, body) => {
                let mut context = context.clone();
                context.push_front(arg.clone());
                format!("(λ{}. {})", arg, &body.print(context, global))
            }
            Term::Application(lhs, rhs) => format!(
                "({} {})",
                &lhs.print(context.clone(), global),
                &rhs.print(context, global)
            ),
            Term::Variable(idx) => {
                let depth = idx.depth() as usize;
                if depth >= context.len() {
                    format!("{}", global[&idx])
                } else {
                    format!("{}", context[depth])
                }
            }
        }
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

#[extend::ext]
pub impl (Term, GlobalContext) {
    fn print(&self) -> String {
        self.0.print(Vector::new(), &self.1)
    }
}

#[cfg(test)]
pub(crate) mod test {
    use super::*;
    use crate::parse::term;

    macro_rules! assert_lowers {
        ($thing:expr, $expect:expr) => {
            ::pretty_assertions::assert_eq!($thing.unwrap().1.lower().0, $expect)
        };
        ($($thing: expr => $expect: expr),* $(,)?) => {
            $(
                assert_lowers!($thing, $expect);
            )*
        };
    }

    macro_rules! assert_prints {
        ($thing: expr, $expect: expr) => {
            ::pretty_assertions::assert_eq!(
                $thing
                    .unwrap()
                    .1
                    .lower()
                    .print()
                    .as_str(),
                $expect
            )
        };
        ($(thing: expr => $expect: expr),* $(,)?) => {
            $(
                assert_prints!(thing, $expect);
            )*
        };
    }

    macro_rules! t_ {
        ($ident: ident) => {
            String::from(stringify!($ident))
        };
        ($idx: expr) => {
            $crate::ir::Term::Variable($crate::debruijn::DebruijnIndex::new($idx))
        };
        ($v: ident -> $b: expr) => {
            $crate::ir::Term::Abstraction(String::from(stringify!($v)), Box::new($b.into()))
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
                        assert_lowers!($a, $b);
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
                        assert_prints!($a, $b);
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
    }
}
