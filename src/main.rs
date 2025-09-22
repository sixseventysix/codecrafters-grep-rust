mod regex;
mod parser;

use std::env;
use std::io;
use std::process;
use anyhow::{Context, Result};

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        anyhow::bail!("Usage: {} -E <pattern>", args[0]);
    }

    if args[1] != "-E" {
        anyhow::bail!("Expected first argument to be '-E', got '{}'", args[1]);
    }

    let pattern = &args[2];
    println!("Input pattern: '{}'", pattern);

    let mut input_line = String::new();
    io::stdin()
        .read_line(&mut input_line)
        .context("Failed to read input from stdin")?;

    println!("Input text: '{}'", input_line.trim());

    let regex = parser::parse_regex(pattern)
        .context("Failed to parse regex pattern")?;

    let matches = regex.find_match(input_line.trim_end());
    println!("Match result: {}", matches);

    if matches {
        process::exit(0);
    } else {
        process::exit(1);
    }
}
