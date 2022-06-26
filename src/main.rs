use std::io::{self, Read};

use lambda_mac::eval::EvalContext;
use tracing::dispatcher::DefaultGuard;
use tracing_subscriber::{prelude::*, EnvFilter, Registry};
use tracing_tree::HierarchicalLayer;

#[allow(dead_code)]
fn init_tracing() -> DefaultGuard {
    let subscriber = Registry::default()
        .with(EnvFilter::from_env("RUST_LOG"))
        .with(HierarchicalLayer::new(2).with_indent_lines(true));
    tracing::subscriber::set_default(subscriber)
}

fn main() {
    let _tracing = init_tracing();
    let mut buf = String::new();
    let _ = io::stdin().read_to_string(&mut buf).unwrap();

    let (_, program) = lambda_mac::parse::program(&buf).unwrap();
    let mut name_ctx = lambda_mac::ir::NamingContext::new();
    let mut stmts = Vec::new();
    for stmt in program {
        stmts.push(name_ctx.lower_stmt(stmt));
    }
    let eval_ctx = EvalContext::new(stmts);
    eval_ctx.eval(true);
}
