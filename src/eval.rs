use std::ops::ControlFlow;

use crate::ir::Term;

impl Term {
    #[tracing::instrument]
    pub fn eval(self) -> Term {
        #[tracing::instrument]
        fn eval1(term: Term) -> ControlFlow<Term, Term> {
            match term {
                Term::Application(lhs, rhs) if lhs.is_value() && rhs.is_value() => {
                    let body = lhs.body().unwrap().clone();
                    ControlFlow::Continue(body.substituted(*rhs))
                }
                Term::Application(lhs, rhs) if lhs.is_value() => {
                    ControlFlow::Continue(Term::Application(lhs, Box::new(eval1(*rhs)?)))
                }
                Term::Application(lhs, rhs) => {
                    ControlFlow::Continue(Term::Application(Box::new(eval1(*lhs)?), rhs))
                }
                _ => ControlFlow::Break(term),
            }
        }

        let mut current = self;
        loop {
            match eval1(current) {
                ControlFlow::Continue(term) => {
                    current = term;
                }
                ControlFlow::Break(term) => return term,
            }
        }
    }
}

#[cfg(test)]
mod test {

    #[allow(dead_code)]
    fn init_tracing() -> tracing::dispatcher::DefaultGuard {
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{EnvFilter, Registry};
        use tracing_tree::HierarchicalLayer;
        let subscriber = Registry::default()
            .with(EnvFilter::from_env("RUST_LOG"))
            .with(HierarchicalLayer::new(2).with_indent_lines(true));
        tracing::subscriber::set_default(subscriber)
    }

    macro_rules! eval_tests {
        ($($name: ident : $tm: expr => $exp: expr);* $(;)?) => {
            paste::paste! {
                $(
                    #[test]
                    fn [<test_eval_ $name>]() {
                        let (term_parsed, ctx) = $crate::parse::term($tm).unwrap().1.lower();
                        ::pretty_assertions::assert_str_eq!(term_parsed.eval().print(::im::Vector::new(), &ctx), $exp);
                    }
                )*
            }
        };
    }

    eval_tests! {
        eval_basic  : "(λ x. x) (λ y. y)" => "(λy. y)";
        eval_basic2 : "((λx. λy. x) y) z" => "y";
    }
}
