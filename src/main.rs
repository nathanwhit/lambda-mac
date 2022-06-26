use lambda_mac::eval::EvalContext;
use nom::{error::convert_error, Finish};
use rustyline::Editor;
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

fn main() -> rustyline::Result<()> {
    let _tracing = init_tracing();
    let mut rl = Editor::<()>::new();
    let mut name_ctx = lambda_mac::ir::NamingContext::new();
    let mut eval_ctx = EvalContext::new(vec![]);
    loop {
        let line = rl.readline("> ")?;
        if line.trim() == "print" {
            eval_ctx.print_globals();
            continue;
        }
        let stmts = match lambda_mac::parse::program(&line).finish() {
            Ok((_, stmts)) => stmts,
            Err(e) => {
                eprintln!("{}", convert_error(&*line, e));
                continue;
            }
        };
        let lowered = name_ctx.lower_stmts(stmts);
        eval_ctx.load(lowered);
        eval_ctx.eval(true);
        rl.add_history_entry(line);
    }
}
