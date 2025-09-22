mod regex;
mod parser;

use std::env;
use std::fs;
use std::io;
use std::process;
use anyhow::{Context, Result};

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        anyhow::bail!("Usage: {} -E <pattern> [file]", args[0]);
    }

    if args[1] != "-E" {
        anyhow::bail!("Expected first argument to be '-E', got '{}'", args[1]);
    }

    let pattern = &args[2];

    let regex = parser::parse_regex(pattern)
        .context("Failed to parse regex pattern")?;

    let mut found_match = false;

    if args.len() >= 4 {
        let filename = &args[3];
        let file_content = fs::read_to_string(filename)
            .context(format!("Failed to read file '{}'", filename))?;

        for line in file_content.lines() {
            if regex.find_match(line) {
                println!("{}", line);
                found_match = true;
            }
        }
    } else {
        let mut input_line = String::new();
        io::stdin()
            .read_line(&mut input_line)
            .context("Failed to read input from stdin")?;

        let line = input_line.trim_end();
        if regex.find_match(line) {
            println!("{}", line);
            found_match = true;
        }
    }

    if found_match {
        process::exit(0);
    } else {
        process::exit(1);
    }
}
