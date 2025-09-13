use std::env;
use std::io;
use std::process;
use std::collections::HashSet;
use std::iter::Peekable;

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    StartAnchor,
    EndAnchor,
    Digit,
    Word,
    Lit(char),
    Group { set: HashSet<char>, neg: bool },
    OneOrMore(Box<Pattern>),
    ZeroOrMore(Box<Pattern>),
    ZeroOrOne(Box<Pattern>),
}

impl Pattern {
    pub fn is_equivalent(&self, other: &Pattern) -> bool {
        match (self, other) {
            (Pattern::StartAnchor, Pattern::StartAnchor) => true,
            (Pattern::EndAnchor, Pattern::EndAnchor) => true,
            (Pattern::Digit, Pattern::Digit) => true,
            (Pattern::Word, Pattern::Word) => true,
            (Pattern::Lit(c1), Pattern::Lit(c2)) => c1 == c2,
            (Pattern::Group { set: s1, neg: n1 }, Pattern::Group { set: s2, neg: n2 }) => {
                s1 == s2 && n1 == n2
            },
            (Pattern::OneOrMore(inner1), Pattern::OneOrMore(inner2)) => {
                inner1.is_equivalent(inner2)
            },
            (Pattern::ZeroOrMore(inner1), Pattern::ZeroOrMore(inner2)) => {
                inner1.is_equivalent(inner2)
            },
            (Pattern::ZeroOrOne(inner1), Pattern::ZeroOrOne(inner2)) => {
                inner1.is_equivalent(inner2)
            },
            _ => false,
        }
    }
}

pub struct Parser;

#[derive(Debug)]
pub enum ParseError {
    DanglingBackslash,
    DanglingPlus,
    UnclosedGroup,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::DanglingBackslash => write!(f, "dangling backslash in pattern"),
            ParseError::DanglingPlus => write!(f, "dangling '+' operator"),
            ParseError::UnclosedGroup => write!(f, "unclosed character group"),
        }
    }
}

impl Parser {
    pub fn parse(pattern: &str) -> Result<Vec<Pattern>, ParseError> {
        let mut chars = pattern.chars().peekable();
        let mut patterns = Vec::new();

        if chars.peek() == Some(&'^') {
            patterns.push(Pattern::StartAnchor);
            chars.next();
        }

        while let Some(&ch) = chars.peek() {
            let pattern = match ch {
                '\\' => Self::parse_escape(&mut chars)?,
                '[' => Self::parse_group(&mut chars)?,
                '$' => Self::parse_dollar(&mut chars),
                _ => Self::parse_literal(&mut chars),
            };

            patterns.push(pattern);

            match chars.peek() {
                Some('+') => {
                    chars.next();
                    let last_pattern = patterns.pop().ok_or(ParseError::DanglingPlus)?;
                    patterns.push(Pattern::OneOrMore(Box::new(last_pattern)));
                },
                Some('*') => {
                    chars.next();
                    let last_pattern = patterns.pop().ok_or(ParseError::DanglingPlus)?;
                    patterns.push(Pattern::ZeroOrMore(Box::new(last_pattern)));
                },
                Some('?') => {
                    chars.next();
                    let last_pattern = patterns.pop().ok_or(ParseError::DanglingPlus)?;
                    patterns.push(Pattern::ZeroOrOne(Box::new(last_pattern)));
                },
                _ => {},
            }
        }

        Self::cleanup_end_anchor(&mut patterns);

        Ok(patterns)
    }

    fn parse_escape(chars: &mut Peekable<std::str::Chars>) -> Result<Pattern, ParseError> {
        chars.next();
        match chars.next() {
            Some('d') => Ok(Pattern::Digit),
            Some('w') => Ok(Pattern::Word),
            Some(c) => Ok(Pattern::Lit(c)),
            None => Err(ParseError::DanglingBackslash),
        }
    }

    fn parse_group(chars: &mut Peekable<std::str::Chars>) -> Result<Pattern, ParseError> {
        chars.next();
        
        let negated = if chars.peek() == Some(&'^') {
            chars.next();
            true
        } else {
            false
        };

        let mut set = HashSet::new();
        while let Some(ch) = chars.next() {
            match ch {
                ']' => return Ok(Pattern::Group { set, neg: negated }),
                '\\' => {
                    if let Some(escaped) = chars.next() {
                        set.insert(escaped);
                    }
                },
                c => {
                    set.insert(c);
                },
            };
        }
        
        Err(ParseError::UnclosedGroup)
    }

    fn parse_dollar(chars: &mut Peekable<std::str::Chars>) -> Pattern {
        chars.next();
        if chars.peek().is_some() {
            Pattern::Lit('$')
        } else {
            Pattern::EndAnchor
        }
    }

    fn parse_literal(chars: &mut Peekable<std::str::Chars>) -> Pattern {
        let ch = chars.next().unwrap();
        Pattern::Lit(ch)
    }

    fn cleanup_end_anchor(patterns: &mut Vec<Pattern>) {
        if let Some(Pattern::Lit('$')) = patterns.last() {
            patterns.pop();
            patterns.push(Pattern::EndAnchor);
        }
    }
}

pub struct Matcher<'a> {
    input: &'a [char],
    patterns: &'a [Pattern],
    debug: bool,
}

#[derive(Debug)]
pub struct MatchContext {
    pub input_idx: usize,
    pub pattern_idx: usize,
}

impl<'a> Matcher<'a> {
    pub fn new(input: &'a [char], patterns: &'a [Pattern]) -> Self {
        Self { input, patterns, debug: false }
    }

    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
    }

    pub fn find_match(&self) -> bool {
        if self.patterns.is_empty() {
            return false;
        }

        let start_positions = self.get_start_positions();
        
        for start_pos in start_positions {
            if self.debug {
                println!("\nAttempting match starting at index {} (char '{}')", 
                    start_pos, self.input.get(start_pos).unwrap_or(&' '));
            }
            
            if self.match_from_position(start_pos) {
                if self.debug {
                    println!("\n--- Full match found! ---");
                }
                return true;
            }
        }
        
        false
    }

    fn get_start_positions(&self) -> Vec<usize> {
        if matches!(self.patterns.first(), Some(Pattern::StartAnchor)) {
            vec![0]
        } else {
            (0..=self.input.len()).collect()
        }
    }

    fn match_from_position(&self, start_pos: usize) -> bool {
        let mut input_idx = start_pos;
        
        for (pattern_idx, pattern) in self.patterns.iter().enumerate() {
            let start_idx = input_idx;
            let remaining_patterns = &self.patterns[pattern_idx + 1..];
            
            if !self.match_pattern(&mut input_idx, pattern, remaining_patterns) {
                if self.debug {
                    println!("  -> Pattern failed to match at index {}", start_idx);
                }
                return false;
            } else if self.debug {
                println!("  -> Pattern matched! New index is {}", input_idx);
            }
        }
        
        true
    }

    fn match_pattern(&self, input_idx: &mut usize, pattern: &Pattern, remaining_patterns: &[Pattern]) -> bool {
        match pattern {
            Pattern::StartAnchor => *input_idx == 0,
            Pattern::EndAnchor => *input_idx == self.input.len(),
            Pattern::Digit => self.match_digit(input_idx),
            Pattern::Word => self.match_word(input_idx),
            Pattern::Lit(c) => self.match_literal(input_idx, *c),
            Pattern::Group { set, neg } => self.match_group(input_idx, set, *neg),
            Pattern::OneOrMore(inner) => self.match_one_or_more(input_idx, inner, remaining_patterns),
            Pattern::ZeroOrMore(inner) => self.match_zero_or_more(input_idx, inner, remaining_patterns),
            Pattern::ZeroOrOne(inner) => self.match_zero_or_one(input_idx, inner, remaining_patterns),
        }
    }

    fn match_digit(&self, input_idx: &mut usize) -> bool {
        if *input_idx < self.input.len() && self.input[*input_idx].is_ascii_digit() {
            *input_idx += 1;
            true
        } else {
            false
        }
    }

    fn match_word(&self, input_idx: &mut usize) -> bool {
        if *input_idx < self.input.len() {
            let ch = self.input[*input_idx];
            if ch.is_alphanumeric() || ch == '_' {
                *input_idx += 1;
                return true;
            }
        }
        false
    }

    fn match_literal(&self, input_idx: &mut usize, literal: char) -> bool {
        if *input_idx < self.input.len() && self.input[*input_idx] == literal {
            *input_idx += 1;
            true
        } else {
            false
        }
    }

    fn match_group(&self, input_idx: &mut usize, set: &HashSet<char>, negated: bool) -> bool {
        if *input_idx < self.input.len() {
            let ch = self.input[*input_idx];
            let in_set = set.contains(&ch);
            if (negated && !in_set) || (!negated && in_set) {
                *input_idx += 1;
                return true;
            }
        }
        false
    }

    fn match_one_or_more(&self, input_idx: &mut usize, inner: &Pattern, remaining_patterns: &[Pattern]) -> bool {
        let original_idx = *input_idx;
        let mut matched_count: usize = 0;

        while self.match_pattern(input_idx, inner, &[]) {
            matched_count += 1;
        }

        if matched_count == 0 {
            *input_idx = original_idx;
            return false;
        }

        let reserved_count = self.count_equivalent_patterns(inner, remaining_patterns);
        let usable_count = matched_count.saturating_sub(reserved_count);

        if usable_count == 0 {
            *input_idx = original_idx;
            return false;
        }

        *input_idx = original_idx;
        for _ in 0..usable_count {
            self.match_pattern(input_idx, inner, &[]);
        }

        true
    }

    fn match_zero_or_more(&self, input_idx: &mut usize, inner: &Pattern, remaining_patterns: &[Pattern]) -> bool {
        let original_idx = *input_idx;
        let mut matched_count: usize = 0;

        while self.match_pattern(input_idx, inner, &[]) {
            matched_count += 1;
        }

        let reserved_count = self.count_equivalent_patterns(inner, remaining_patterns);
        let usable_count = matched_count.saturating_sub(reserved_count);

        *input_idx = original_idx;
        for _ in 0..usable_count {
            self.match_pattern(input_idx, inner, &[]);
        }

        true
    }

    fn match_zero_or_one(&self, input_idx: &mut usize, inner: &Pattern, remaining_patterns: &[Pattern]) -> bool {
        let original_idx = *input_idx;

        if self.match_pattern(input_idx, inner, &[]) {
            let reserved_count = self.count_equivalent_patterns(inner, remaining_patterns);
            if reserved_count > 0 {
                *input_idx = original_idx;
            }
        }
        
        true
    }

    fn count_equivalent_patterns(&self, target: &Pattern, patterns: &[Pattern]) -> usize {
        patterns.iter()
            .take_while(|&p| target.is_equivalent(p))
            .count()
    }
}

pub struct RegexEngine {
    patterns: Vec<Pattern>,
    debug: bool,
}

impl RegexEngine {
    pub fn new(pattern_str: &str) -> Result<Self, ParseError> {
        let patterns = Parser::parse(pattern_str)?;
        Ok(Self { patterns, debug: false })
    }

    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
    }

    pub fn is_match(&self, input: &str) -> bool {
        let chars: Vec<char> = input.chars().collect();
        
        if self.debug {
            println!("Pattern: {:?}", self.patterns);
        }
        
        Matcher::new(&chars, &self.patterns)
            .with_debug(self.debug)
            .find_match()
    }

    pub fn patterns(&self) -> &[Pattern] {
        &self.patterns
    }
}

// Usage: echo <input_text> | your_program.sh -E <pattern>
fn main() {
    if env::args().nth(1).unwrap() != "-E" {
        eprintln!("Expected first argument to be '-E'");
        process::exit(1);
    }

    let pattern = env::args().nth(2).unwrap();
    let mut input_line = String::new();
    io::stdin().read_line(&mut input_line).unwrap();

    let engine = match RegexEngine::new(&pattern) {
        Ok(engine) => engine.with_debug(true),
        Err(e) => {
            eprintln!("Pattern compilation error: {}", e);
            process::exit(1);
        }
    };

    if engine.is_match(&input_line) {
        process::exit(0);
    } else {
        process::exit(1);
    }
}
