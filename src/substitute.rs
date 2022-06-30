use crate::{debruijn::DebruijnIndex, ir::Term};

impl Term {
    #[tracing::instrument]
    pub fn substituted(self, subst: Term) -> Term {
        let subst_shifted = subst.shifted_in(DebruijnIndex::ONE);
        let substituted = self.substituted_(DebruijnIndex::INNERMOST, subst_shifted);
        substituted.shifted_out(DebruijnIndex::ONE).unwrap()
        // substituted
    }

    #[tracing::instrument]
    fn substituted_(self, target: DebruijnIndex, subst: Term) -> Term {
        #[tracing::instrument]
        fn walk(term: Term, target: DebruijnIndex, outer: DebruijnIndex, subst: Term) -> Term {
            match term {
                Term::Variable(idx, len) => {
                    if idx.depth() == target.depth() + outer.depth() {
                        tracing::debug!(?subst, ?outer, ?idx, "shifting in");
                        subst.shifted_in(outer)
                    } else {
                        Term::Variable(idx, len)
                    }
                }
                Term::Abstraction(name, body) => Term::Abstraction(
                    name,
                    Box::new(walk(*body, target, outer.shifted_in(), subst)),
                ),
                Term::Application(lhs, rhs) => Term::Application(
                    Box::new(walk(*lhs, target, outer, subst.clone())),
                    Box::new(walk(*rhs, target, outer, subst)),
                ),
                Term::Let(id, value, body) => Term::Let(
                    id,
                    Box::new(walk(*value, target, outer, subst.clone())),
                    Box::new(walk(*body, target, outer.shifted_in(), subst)),
                ),
            }
        }

        walk(self, target, DebruijnIndex::INNERMOST, subst)
    }

    pub fn substituted_with(self, target: DebruijnIndex, subst: Term) -> Term {
        self.substituted_(target, subst)
    }
}

#[cfg(test)]
mod test {
    use pretty_assertions::assert_eq;

    use super::*;
    use crate::ir::test::t;

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
    fn basic_subst() {
        let _foo = init_tracing();
        assert_eq!(t!(0).substituted_(DebruijnIndex::INNERMOST, t!(3)), t!(3));
    }
}
