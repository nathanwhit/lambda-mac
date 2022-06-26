use std::ops::ControlFlow;

use im::HashMap;

use crate::{
    debruijn::DebruijnIndex,
    ir::{Statement, Term},
};

#[derive(Clone)]
pub struct EvalContext {
    statements: Vec<Statement>,
    global_values: HashMap<DebruijnIndex, Term>,
}

impl EvalContext {
    pub fn new(statements: Vec<Statement>) -> Self {
        Self {
            statements,
            global_values: HashMap::new(),
        }
    }
    pub fn eval(mut self, print: bool) -> Vec<Term> {
        let mut res = Vec::new();
        let statements = std::mem::take(&mut self.statements);
        for Statement { stmt, mut context } in statements {
            match stmt {
                crate::ir::Stmt::Expr(term) => {
                    let res = self.eval_term(term.clone());
                    if print {
                        println!(
                            "{} -> {}",
                            context.print_term(&term),
                            context.print_term(&res)
                        );
                    }
                }
                crate::ir::Stmt::Bind(name, expr) => {
                    let idx = context
                        .index_of(&name)
                        .expect(&format!("unbound ident {name:?}"));
                    let value = self.eval_term(expr.clone());
                    tracing::debug!("{name} = {}", context.print_term(&value));
                    self.global_values.insert(idx, value.clone());
                    if print {
                        println!(
                            "{name} = {} -> {name} = {}",
                            context.print_term(&expr),
                            context.print_term(&value)
                        );
                    }
                    res.push(value);
                }
            }
        }
        res
    }

    pub fn eval_term(&mut self, term: Term) -> Term {
        fn eval1(ctx: &mut EvalContext, term: Term) -> ControlFlow<Term, Term> {
            match term {
                Term::Application(lhs, rhs) if lhs.is_value() && rhs.is_value() => {
                    let body = lhs.body().unwrap().clone();
                    ControlFlow::Continue(body.substituted(*rhs))
                }
                Term::Application(lhs, rhs) if lhs.is_value() => {
                    ControlFlow::Continue(Term::Application(lhs, Box::new(eval1(ctx, *rhs)?)))
                }
                Term::Application(lhs, rhs) => {
                    ControlFlow::Continue(Term::Application(Box::new(eval1(ctx, *lhs)?), rhs))
                }
                Term::Variable(idx) => {
                    if let Some(val) = ctx.global_values.get(&idx) {
                        return ControlFlow::Continue(val.clone());
                    } else {
                        ControlFlow::Break(term)
                    }
                }
                _ => ControlFlow::Break(term),
            }
        }
        let mut current = term;
        loop {
            match eval1(self, current) {
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
                        let mut ctx = $crate::ir::NamingContext::new();
                        let term_parsed = $crate::parse::term($tm).unwrap().1.lower(&mut ctx);
                        let mut eval_ctx = $crate::eval::EvalContext::new(vec![]);

                        ::pretty_assertions::assert_str_eq!(eval_ctx.eval_term(term_parsed).print(&mut ctx).as_str(), $exp);
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
