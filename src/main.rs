use std::env;
use std::io;
use std::process;
use std::collections::HashSet;

#[derive(Debug)]
enum Pattern {
    Digit,
    Word,
    Lit(String),
    Group { set: HashSet<char>, neg: bool },
}

fn compile_pattern(pat: &str) -> Result<Vec<Pattern>, &'static str> {
    let mut it = pat.chars().peekable();
    let mut patterns = Vec::new();
    let mut literal_buffer = String::new();

    while let Some(c) = it.peek().copied() {
        if c == '\\' {
            it.next();
            if let Some(next_char) = it.next() {
                if !literal_buffer.is_empty() {
                    patterns.push(Pattern::Lit(literal_buffer.drain(..).collect()));
                }
                match next_char {
                    'd' => patterns.push(Pattern::Digit),
                    'w' => patterns.push(Pattern::Word),
                    _ => patterns.push(Pattern::Lit(String::from(next_char))),
                }
            } else {
                return Err("dangling backslash in pattern");
            }
        } else if c == '[' {
            if !literal_buffer.is_empty() {
                patterns.push(Pattern::Lit(literal_buffer.drain(..).collect()));
            }
            it.next();
            patterns.push(compile_group(&mut it)?);
        } else {
            literal_buffer.push(c);
            it.next();
        }
    }
    if !literal_buffer.is_empty() {
        patterns.push(Pattern::Lit(literal_buffer.drain(..).collect()));
    }
    Ok(patterns)
}

fn compile_group<I>(it: &mut std::iter::Peekable<I>) -> Result<Pattern, &'static str>
where
    I: Iterator<Item = char>,
{
    let mut neg = false;
    if let Some('^') = it.peek().copied() {
        it.next();
        neg = true;
    }

    let mut set = HashSet::new();
    let mut lit_storage = String::new();

    while let Some(ch) = it.next() {
        if ch == ']' {

            if !lit_storage.is_empty() {

                return Ok(Pattern::Lit(format!("[{}", lit_storage)));
            }
            return Ok(Pattern::Group { set, neg });
        }

        lit_storage.push(ch);

        if ch == '\\' {
            if let Some(esc) = it.next() {
                set.insert(esc);
            }
        } else {
            set.insert(ch);
        }
    }

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
        Err(_) => return false,
    };
    if patterns.is_empty() {
        return false;
    }

    let chars: Vec<char> = input_line.chars().collect();
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
