use std::ops::Not;

use clap::{Parser, Subcommand};
use color_eyre::eyre::eyre;
use lambda_mac::{eval::EvalContext, expand::Expander};
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

#[derive(Parser)]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Run { path: String },
    Repl,
}

fn repl() -> color_eyre::Result<()> {
    let mut rl = Editor::<()>::new();
    let mut eval_ctx = EvalContext::new(vec![]);
    loop {
        let line = rl.readline("> ")?;
        let stmts = match lambda_mac::parse::program(&line).finish() {
            Ok((_, stmts)) => stmts,
            Err(e) => {
                eprintln!("{}", convert_error(&*line, e));
                rl.add_history_entry(line);
                continue;
            }
        };
        let program = Expander::new(stmts).expand()?;
        eval_ctx.load(program);
        eval_ctx.eval(true)?;
        rl.add_history_entry(line);
    }
}

fn run(path: &str) -> color_eyre::Result<()> {
    let input = std::fs::read_to_string(path)?;
    let (input, program) = lambda_mac::parse::program(&input)
        .finish()
        .map_err(|e| eyre!("parse error: {}", convert_error(&*input, e)))?;
    if input.trim().is_empty().not() {
        return Err(eyre!("input remaining: {input}"));
    }
    let program = Expander::new(program).expand()?;
    let mut eval_ctx = EvalContext::new(program);
    eval_ctx.eval(true)?;
    Ok(())
}

fn main() -> color_eyre::Result<()> {
    color_eyre::install()?;
    let _tracing = init_tracing();

    let cli = Cli::parse();
    match cli.command {
        Commands::Run { path } => run(&path)?,
        Commands::Repl => repl()?,
    }
    Ok(())
}
