mod regex;
mod parser;

use std::env;
use std::fs;
use std::process;
use anyhow::{Context, Result};

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 4 {
        anyhow::bail!("Usage: {} -E <pattern> <file>", args[0]);
    }

    if args[1] != "-E" {
        anyhow::bail!("Expected first argument to be '-E', got '{}'", args[1]);
    }

    let pattern = &args[2];
    let filename = &args[3];

    let input_line = fs::read_to_string(filename)
        .context(format!("Failed to read file '{}'", filename))?;

    let regex = parser::parse_regex(pattern)
        .context("Failed to parse regex pattern")?;

    let line = input_line.trim_end();
    let matches = regex.find_match(line);

    if matches {
        println!("{}", line);
        process::exit(0);
    } else {
        process::exit(1);
    }
}
