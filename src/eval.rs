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

    use im::Vector;

    use crate::ir::test::t;
    use crate::parse::term;

    fn init_tracing() -> tracing::dispatcher::DefaultGuard {
        use tracing_subscriber::prelude::*;
        use tracing_subscriber::{EnvFilter, Registry};
        use tracing_tree::HierarchicalLayer;
        let subscriber = Registry::default()
            .with(EnvFilter::from_env("RUST_LOG"))
            .with(HierarchicalLayer::new(2).with_indent_lines(true));
        tracing::subscriber::set_default(subscriber)
    }

    #[test]
    fn eval_basic() {
        // let _foo = init_tracing();
        pretty_assertions::assert_eq!(
            term("(λ x. x) (λ y. y)").unwrap().1.lower().0.eval(),
            t!(y -> t!(0))
        );
    }

    #[test]
    fn eval_basic2() {
        let _foo = init_tracing();
        let (tm, ctx) = term("((λx. λy. x) y) z").unwrap().1.lower();
        println!("{ctx:?}");
        pretty_assertions::assert_str_eq!(tm.eval().print(Vector::new(), &ctx), "y");
    }
}
