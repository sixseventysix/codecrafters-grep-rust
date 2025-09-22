mod regex;
mod parser;

use std::env;
use std::fs;
use std::io;
use std::path::Path;
use std::process;
use anyhow::{Context, Result};

fn search_file(regex: &regex::Regex, filename: &str, show_filename: bool, found_match: &mut bool) -> Result<()> {
    let file_content = fs::read_to_string(filename)
        .context(format!("Failed to read file '{}'", filename))?;

    for line in file_content.lines() {
        if regex.find_match(line) {
            if show_filename {
                println!("{}:{}", filename, line);
            } else {
                println!("{}", line);
            }
            *found_match = true;
        }
    }
    Ok(())
}

fn search_directory_recursive(regex: &regex::Regex, dir_path: &str, found_match: &mut bool) -> Result<()> {
    let dir = fs::read_dir(dir_path)
        .context(format!("Failed to read directory '{}'", dir_path))?;

    for entry in dir {
        let entry = entry.context("Failed to read directory entry")?;
        let path = entry.path();
        let path_str = path.to_string_lossy();

        if path.is_file() {
            search_file(regex, &path_str, true, found_match)?;
        } else if path.is_dir() {
            search_directory_recursive(regex, &path_str, found_match)?;
        }
    }
    Ok(())
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 3 {
        anyhow::bail!("Usage: {} [-r] -E <pattern> [file/dir...]", args[0]);
    }

    let mut arg_index = 1;
    let mut recursive = false;

    // Check for -r flag
    if args[arg_index] == "-r" {
        recursive = true;
        arg_index += 1;

        if args.len() < 4 {
            anyhow::bail!("Usage: {} [-r] -E <pattern> [file/dir...]", args[0]);
        }
    }

    if args[arg_index] != "-E" {
        anyhow::bail!("Expected '-E' flag, got '{}'", args[arg_index]);
    }
    arg_index += 1;

    let pattern = &args[arg_index];
    arg_index += 1;

    let regex = parser::parse_regex(pattern)
        .context("Failed to parse regex pattern")?;

    let mut found_match = false;

    if arg_index < args.len() {
        // File/directory mode
        let paths = &args[arg_index..];
        let multiple_paths = paths.len() > 1;

        for path in paths {
            let path_obj = Path::new(path);

            if recursive && path_obj.is_dir() {
                search_directory_recursive(&regex, path, &mut found_match)?;
            } else if path_obj.is_file() {
                search_file(&regex, path, multiple_paths || recursive, &mut found_match)?;
            } else if path_obj.is_dir() && !recursive {
                anyhow::bail!("Directory '{}' specified but -r flag not provided", path);
            } else {
                anyhow::bail!("Path '{}' does not exist or is not a file/directory", path);
            }
        }
    } else {
        // Stdin mode
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
