use std::ops::ControlFlow;

use crate::{
    ast::Stmt,
    ir::{BindingContext, Term},
};

#[derive(Clone, Debug)]
pub struct EvalContext {
    statements: Vec<crate::ast::Stmt>,
    bindings: BindingContext,
}

impl EvalContext {
    pub fn new(statements: Vec<Stmt>) -> Self {
        Self {
            statements,
            bindings: BindingContext::new(),
        }
    }
    pub fn empty() -> Self {
        Self {
            statements: Vec::new(),
            bindings: BindingContext::new(),
        }
    }
    pub fn load(&mut self, statements: impl IntoIterator<Item = Stmt>) {
        self.statements.extend(statements);
    }
    #[tracing::instrument]
    pub fn eval(&mut self, print: bool) -> Vec<Term> {
        let mut res = Vec::new();
        let statements = std::mem::take(&mut self.statements);
        for stmt in statements {
            tracing::debug!(?stmt, "executing statement");
            match stmt {
                Stmt::Expr(term) => {
                    let term = term.lower(&mut self.bindings);
                    tracing::debug!(?self.bindings, "evaluating expr {}", self.bindings.print_term(&term));
                    let res = self.eval_term(term.clone());
                    tracing::debug!("evaluated: {}", self.bindings.print_term(&res));
                    if print {
                        println!("{}", self.bindings.print_term(&res));
                    }
                }
                Stmt::Bind(name, expr) => {
                    let expr = expr.lower(&mut self.bindings);
                    let value = self.eval_term(expr);
                    tracing::debug!("{name} = {}", self.bindings.print_term(&value));
                    if print {
                        println!("{name} = {}", self.bindings.print_term(&value));
                    }
                    self.bindings.add_global(name.clone(), value.clone());
                    res.push(value);
                }
                Stmt::Import(_) => todo!(),
            }
        }
        res
    }

    #[tracing::instrument(skip(self))]
    pub fn eval_term(&mut self, term: Term) -> Term {
        #[tracing::instrument(skip(ctx))]
        fn eval1(ctx: &mut EvalContext, term: Term) -> ControlFlow<(), Term> {
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
                Term::Variable(idx, _) => {
                    if let Some(val) = ctx.bindings.get_global(idx) {
                        tracing::debug!(
                            ?val,
                            value = %ctx.bindings.print_term(&val),
                            "continuing with global"
                        );
                        return ControlFlow::Continue(val);
                    } else {
                        ControlFlow::Break(())
                    }
                }
                Term::Let(_, value, body) if value.is_value() => {
                    ControlFlow::Continue(body.substituted(*value))
                }
                Term::Let(id, value, body) => {
                    ControlFlow::Continue(Term::Let(id, Box::new(eval1(ctx, *value)?), body))
                }
                _ => ControlFlow::Break(()),
            }
        }
        match eval1(self, term.clone()) {
            ControlFlow::Continue(term) => self.eval_term(term),
            ControlFlow::Break(()) => term,
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
                        let mut ctx = $crate::ir::BindingContext::new();
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
        and         : "(λ b. λ c. b c (λt. λf. t)) (λt. λf. t) (λt. λf. f)" => "(λt. (λf. f))"
    }
}
