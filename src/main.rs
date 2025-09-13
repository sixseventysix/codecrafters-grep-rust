use std::env;
use std::io;
use std::process;
use std::collections::HashSet;
use std::iter::Peekable;

#[derive(Debug, Clone)]
enum Pattern {
    StartAnchor,
    EndAnchor,
    Digit,
    Word,
    Lit(char),
    Group { set: HashSet<char>, neg: bool },
    OneOrMore(Box<Pattern>)
}

fn patterns_equivalent(p1: &Pattern, p2: &Pattern) -> bool {
    match (p1, p2) {
        (Pattern::StartAnchor, Pattern::StartAnchor) => true,
        (Pattern::EndAnchor, Pattern::EndAnchor) => true,
        (Pattern::Digit, Pattern::Digit) => true,
        (Pattern::Word, Pattern::Word) => true,
        (Pattern::Lit(c1), Pattern::Lit(c2)) => c1 == c2,
        (Pattern::Group { set: s1, neg: n1 }, Pattern::Group { set: s2, neg: n2 }) => {
            s1 == s2 && n1 == n2
        },
        (Pattern::OneOrMore(inner1), Pattern::OneOrMore(inner2)) => {
            patterns_equivalent(inner1, inner2)
        },
        _ => false,
    }
}

fn compile_pattern(pat: &str) -> Result<Vec<Pattern>, &'static str> {
    let mut it = pat.chars().peekable();
    let mut patterns = Vec::new();

    if let Some('^') = it.peek().copied() {
        patterns.push(Pattern::StartAnchor);
        it.next();
    }

    while let Some(c) = it.peek().copied() {
        match c {
            '\\' => {
                it.next();
                match it.next() {
                    Some('d') => patterns.push(Pattern::Digit),
                    Some('w') => patterns.push(Pattern::Word),
                    Some(x) => patterns.push(Pattern::Lit(x)),
                    None => return Err("dangling backslash in pattern"),
                }
            },
            '[' => {
                it.next();
                let group = compile_group(&mut it)?;
                patterns.push(group);
            },
            '$' => {
                it.next();
                if it.peek().is_some() {
                    patterns.push(Pattern::Lit('$'));
                } else {
                    patterns.push(Pattern::EndAnchor);
                }
            },
            _ => {
                patterns.push(Pattern::Lit(c));
                it.next();
            }
        }

        if let Some('+') = it.peek().copied() {
            it.next();
            let last_pattern = patterns.pop().ok_or("Dangling '+'")?;
            patterns.push(Pattern::OneOrMore(Box::new(last_pattern)));
        }
    }

    if let Some(Pattern::Lit(last_char)) = patterns.last() {
        if *last_char == '$' {
            patterns.pop();
            patterns.push(Pattern::EndAnchor);
        }
    }

    Ok(patterns)
}

fn compile_group<I>(it: &mut Peekable<I>) -> Result<Pattern, &'static str>
where
    I: Iterator<Item = char>,
{
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

fn matches_token(chars: &[char], input_idx: &mut usize, pattern: &Pattern, remaining_patterns: &[Pattern]) -> bool {
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
        },
        Pattern::Word => {
            if *input_idx < chars.len() && (chars[*input_idx].is_alphanumeric() || chars[*input_idx] == '_') {
                *input_idx += 1;
                true
            } else {
                false
            }
        },
        Pattern::Lit(c) => {
            if *input_idx < chars.len() && chars[*input_idx] == *c {
                *input_idx += 1;
                true
            } else {
                false
            }
        },
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
        },
        Pattern::OneOrMore(inner) => {
            let mut matched_count: usize = 0;
            let original_idx = *input_idx;
            
            while matches_token(chars, input_idx, &**inner, &[]) {
                matched_count += 1;
            }

            if matched_count == 0 {
                *input_idx = original_idx;
                return false;
            }

            let mut reserved_count = 0;
            for remaining_pattern in remaining_patterns {
                if patterns_equivalent(remaining_pattern, &**inner) {
                    reserved_count += 1;
                } else {
                    break;
                }
            }

            let usable_count = matched_count.saturating_sub(reserved_count);
            
            if usable_count == 0 {
                *input_idx = original_idx;
                return false;
            }

            *input_idx = original_idx;
            for _ in 0..usable_count {
                matches_token(chars, input_idx, &**inner, &[]);
            }
            
            true
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
    println!("Compiled Patterns: {:?}", patterns);

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
        
        for (pattern_idx, p) in patterns.iter().enumerate() {
            let start_idx = input_idx;
            let remaining = &patterns[pattern_idx + 1..];

            if !matches_token(&chars, &mut input_idx, p, remaining) {
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
