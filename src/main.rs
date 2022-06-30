use clap::{Parser, Subcommand};
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

fn repl() -> rustyline::Result<()> {
    let mut rl = Editor::<()>::new();
    let mut eval_ctx = EvalContext::new(vec![]);
    loop {
        let line = rl.readline("> ")?;
        // if line.trim() == "print" {
        //     // eval_ctx.print_globals();
        //     rl.add_history_entry(line);
        //     continue;
        // }
        let stmts = match lambda_mac::parse::program(&line).finish() {
            Ok((_, stmts)) => stmts,
            Err(e) => {
                eprintln!("{}", convert_error(&*line, e));
                rl.add_history_entry(line);
                continue;
            }
        };
        eval_ctx.load(stmts);
        eval_ctx.eval(true);
        rl.add_history_entry(line);
    }
}

fn run(path: &str) -> color_eyre::Result<()> {
    let input = std::fs::read_to_string(path)?;
    let (_, program) = lambda_mac::parse::program(&input)
        .finish()
        .map_err(|e| color_eyre::eyre::eyre!("parse error: {}", convert_error(&*input, e)))?;
    let mut eval_ctx = EvalContext::new(program);
    eval_ctx.eval(true);
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
