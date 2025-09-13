use std::env;
use std::io;
use std::process;
use std::collections::HashSet;

enum Pattern {
    Digit,
    Word,
    Lit(String),
    Group { set: HashSet<char>, neg: bool },
}

fn compile_pattern(pat: &str) -> Result<Vec<Pattern>, &'static str> {
    let mut it = pat.chars().peekable();
    let mut patterns = Vec::new();

    while let Some(c) = it.peek().copied() {
        if c == '\\' {
            it.next(); // Consume '\\'
            match it.next() {
                Some('d') => patterns.push(Pattern::Digit),
                Some('w') => patterns.push(Pattern::Word),
                Some(x) => {
                    let mut lit_str = String::from("\\");
                    lit_str.push(x);
                    patterns.push(Pattern::Lit(lit_str));
                }
                None => return Err("dangling backslash in pattern"),
            }
        } else if c == '[' {
            it.next(); // Consume '['
            patterns.push(compile_group(&mut it)?);
        } else {
            let lit_str: String = it
                .by_ref()
                .take_while(|&ch| ch != '\\' && ch != '[')
                .collect();
            patterns.push(Pattern::Lit(lit_str));
        }
    }
    Ok(patterns)
}

fn compile_group<I>(it: &mut std::iter::Peekable<I>) -> Result<Pattern, &'static str>
where
    I: Iterator<Item = char>,
{
    let mut neg = false;
    if let Some('^') = it.peek().copied() {
        it.next(); // Consume '^'
        neg = true;
    }

    let mut set = HashSet::new();
    let mut lit_storage = String::new(); // Store chars if group is unclosed

    while let Some(ch) = it.next() {
        if ch == ']' {
            // If we have an unclosed group, we need to handle that first
            if !lit_storage.is_empty() {
                // This shouldn't happen with the current logic, but as a safeguard.
                return Ok(Pattern::Lit(format!("[{}", lit_storage)));
            }
            return Ok(Pattern::Group { set, neg });
        }
        
        // Accumulate characters just in case the group is not closed
        lit_storage.push(ch);

        if ch == '\\' {
            if let Some(esc) = it.next() {
                set.insert(esc);
            }
        } else {
            set.insert(ch);
        }
    }
    // If loop finishes without finding ']', the group is unclosed
    Err("unclosed character group")
}

fn matches_token(chars: &[char], input_idx: &mut usize, pattern: &Pattern) -> bool {
    match pattern {
        Pattern::Digit => {
            if *input_idx < chars.len() && chars[*input_idx].is_ascii_digit() {
                *input_idx += 1;
                true
            } else {
                false
            }
        }
        Pattern::Word => {
            if *input_idx < chars.len() && (chars[*input_idx].is_alphanumeric() || chars[*input_idx] == '_') {
                *input_idx += 1;
                true
            } else {
                false
            }
        }
        Pattern::Lit(s) => {
            if *input_idx + s.len() <= chars.len() && chars[*input_idx..*input_idx + s.len()].iter().collect::<String>() == *s {
                *input_idx += s.len();
                true
            } else {
                false
            }
        }
        Pattern::Group { set, neg } => {
            if *input_idx < chars.len() {
                let hit = set.contains(&chars[*input_idx]);
                if (*neg && !hit) || (!*neg && hit) {
                    *input_idx += 1;
                    true
                } else {
                    false
                }
            } else {
                false
            }
        }
    }
}

fn match_pattern(input_line: &str, pattern: &str) -> bool {
    let patterns = match compile_pattern(pattern) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Error compiling pattern: {}", e);
            return false;
        }
    };

    if patterns.is_empty() {
        return false;
    }

    let chars: Vec<char> = input_line.chars().collect();
    if chars.is_empty() {
        return patterns.is_empty();
    }

    for i in 0..=chars.len() {
        let mut input_idx = i;
        let mut all_match = true;
        for p in &patterns {
            if !matches_token(&chars, &mut input_idx, p) {
                all_match = false;
                break;
            }
        }
        if all_match {
            return true;
        }
    }
    false
}

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    if env::args().nth(1).unwrap() != "-E" {
        println!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();
    let mut input_line = String::new();

    io::stdin().read_line(&mut input_line).unwrap();

    // Uncomment this block to pass the first stage
    if match_pattern(&input_line, &pattern) {
        process::exit(0)
    } else {
        process::exit(1)
    }
}
