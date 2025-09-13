use std::env;
use std::io;
use std::process;
use std::collections::HashSet;

#[derive(Debug)]
enum Pattern {
    StartAnchor,
    EndAnchor,
    Digit,
    Word,
    Lit(String),
    Group { set: HashSet<char>, neg: bool },
}

fn compile_pattern(pat: &str) -> Result<Vec<Pattern>, &'static str> {
    let mut it = pat.chars().peekable();
    let mut patterns = Vec::new();
    let mut literal_buffer = String::new();

    if let Some('^') = it.peek().copied() {
        patterns.push(Pattern::StartAnchor);
        it.next();
    }

    while let Some(c) = it.peek().copied() {
        match c {
            '\\' => {
                if !literal_buffer.is_empty() {
                    patterns.push(Pattern::Lit(literal_buffer.drain(..).collect()));
                }
                it.next();
                match it.next() {
                    Some('d') => patterns.push(Pattern::Digit),
                    Some('w') => patterns.push(Pattern::Word),
                    Some(x) => patterns.push(Pattern::Lit(x.to_string())),
                    None => return Err("dangling backslash in pattern"),
                }
            },
            '[' => {
                if !literal_buffer.is_empty() {
                    patterns.push(Pattern::Lit(literal_buffer.drain(..).collect()));
                }
                let group_result = compile_group(&mut it);
                match group_result {
                    Ok(p) => patterns.push(p),
                    Err(_) => {
                        let remaining_str: String = it.by_ref().collect();
                        let unclosed_lit = format!("[{}", remaining_str);
                        patterns.push(Pattern::Lit(unclosed_lit));
                        return Ok(patterns);
                    }
                }
            },
            _ => {
                literal_buffer.push(c);
                it.next();
            }
        }
    }
    if literal_buffer.ends_with('$') {
        literal_buffer.pop();
        if !literal_buffer.is_empty() {
            patterns.push(Pattern::Lit(literal_buffer));
        }
        patterns.push(Pattern::EndAnchor);
    } else {
        if !literal_buffer.is_empty() {
            patterns.push(Pattern::Lit(literal_buffer));
        }
    }
    Ok(patterns)
}

fn compile_group<I>(it: &mut std::iter::Peekable<I>) -> Result<Pattern, &'static str>
where
    I: Iterator<Item = char>,
{
    it.next();
    
    let mut neg = false;
    if let Some('^') = it.peek().copied() {
        it.next();
        neg = true;
    }

    let mut set = HashSet::new();
    while let Some(ch) = it.next() {
        if ch == ']' {
            return Ok(Pattern::Group { set, neg });
        }
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
        Pattern::StartAnchor => {
            *input_idx == 0
        },
        Pattern::EndAnchor => {
            *input_idx == chars.len()
        },
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
    println!("Pattern: \"{}\"", pattern);
    print!("Compiled Patterns: {:?}", patterns);

    let chars: Vec<char> = input_line.chars().collect();

    let start_indices: Vec<usize> = if matches!(patterns.first(), Some(Pattern::StartAnchor)) {
        vec![0]
    } else {
        (0..=chars.len()).collect()
    };
    
    for i in start_indices {
        let mut input_idx = i;
        let mut all_match = true;
        println!("\nAttempting match starting at index {} (char '{}')", i, chars.get(i).unwrap_or(&' '));
        
        for p in &patterns {
            let start_idx = input_idx;
            if !matches_token(&chars, &mut input_idx, p) {
                all_match = false;
                println!("  -> Pattern failed to match at index {}", start_idx);
                break;
            } else {
                println!("  -> Pattern matched! New index is {}", input_idx);
            }
        }
        
        if all_match {
            println!("\n--- Full match found! ---");
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
