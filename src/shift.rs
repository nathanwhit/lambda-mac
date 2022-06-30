use std::ops::{Add, Sub};

use crate::{debruijn::DebruijnIndex, ir::Term};

#[derive(Clone, Copy, PartialEq, Debug)]
enum Direction {
    In,
    Out,
}

fn apply<A, B>(original: A, amount: B, direction: Direction) -> <A as Add<B>>::Output
where
    A: Sub<B>,
    A: Add<B, Output = <A as Sub<B>>::Output>,
{
    match direction {
        Direction::In => original + amount,
        Direction::Out => original - amount,
    }
}

impl Term {
    #[tracing::instrument]
    fn shifted(
        self,
        amount: DebruijnIndex,
        outer_binder: DebruijnIndex,
        direction: Direction,
    ) -> Option<Term> {
        Some(match self {
            Term::Abstraction(name, body) => Term::Abstraction(
                name,
                Box::new(body.shifted(amount, outer_binder.shifted_in(), direction)?),
            ),
            Term::Application(lhs, rhs) => Term::Application(
                Box::new(lhs.shifted(amount, outer_binder, direction)?),
                Box::new(rhs.shifted(amount, outer_binder, direction)?),
            ),
            Term::Variable(idx, len) => {
                if idx.within(outer_binder) {
                    Term::Variable(idx, apply(len, amount.depth() as usize, direction))
                } else {
                    match direction {
                        Direction::In => Term::Variable(
                            idx.shifted_in_by(amount),
                            len + (amount.depth() as usize),
                        ),
                        Direction::Out => Term::Variable(
                            idx.shifted_out_by(amount)?,
                            len - (amount.depth() as usize),
                        ),
                    }
                }
            }
            Term::Let(id, value, body) => Term::Let(
                id,
                Box::new(value.shifted(amount, outer_binder, direction)?),
                Box::new(body.shifted(amount, outer_binder.shifted_in(), direction)?),
            ),
        })
    }
    pub fn shifted_in(self, amount: DebruijnIndex) -> Term {
        self.shifted(amount, DebruijnIndex::INNERMOST, Direction::In)
            .unwrap()
    }

    pub fn shifted_out(self, amount: DebruijnIndex) -> Option<Term> {
        self.shifted(amount, DebruijnIndex::INNERMOST, Direction::Out)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ir::test::t;

    fn term(s: &str) -> Term {
        let mut ctx = crate::ir::BindingContext::new();
        ctx.add_free("x".into());
        ctx.add_free("y".into());
        ctx.add_free("z".into());
        crate::parse::term(s).unwrap().1.lower(&mut ctx)
    }

    macro_rules! shifting_tests {
        ($($id : ident : $e: expr => $exp: expr);* $(;)?) => {
            $(
                paste::paste! {
                    #[test]
                    fn [<test_shifting_ $id>]() {
                        pretty_assertions::assert_eq!($e, $exp);
                    }
                }
            )*
        };
    }

    shifting_tests! {
        under_abs   : term("λx. λy. z").shifted_in(DebruijnIndex::ONE)              => t!(x -> t!(y -> t!(3)));
        free_var    : term("z").shifted_in(DebruijnIndex::ONE)                      => t!(1);
        app         : term("((λ x. x) (λ y. y)) z").shifted_in(DebruijnIndex::ONE)  => t!(t!(t!(x -> t!(0)), t!(y -> t!(0))), t!(1));
        out_zero    : term("z").shifted_out(DebruijnIndex::ONE)                     => None;
        out_one     : term("λx. z").shifted_out(DebruijnIndex::ONE)                 => Some(t!(x -> t!(0)));
    }
}
