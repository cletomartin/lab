use clap::{Parser, Subcommand};
use std::path::PathBuf;

/// A CLI application
#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    /// Sets a custom config file
    #[arg(short, long, value_name = "FILE")]
    config: Option<PathBuf>,

    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,

    #[command(subcommand)]
    command: Command,
}


/// A test subcommand
#[derive(Subcommand)]
enum Command {
    /// does testing things
    Test {
        /// lists test values
        #[arg(short, long)]
        list: String,
    },
}


fn main() {
    let cli = Cli::parse();

    match cli.command {
        Command::Test { list } => {
            println!("'test' was used, list is: {list:?}")
        }
    }
}
