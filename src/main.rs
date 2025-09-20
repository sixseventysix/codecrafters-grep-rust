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
    Wildcard,
    Lit(char),
    CharacterClass { set: HashSet<char>, neg: bool },
    OneOrMore(Box<Pattern>),
    ZeroOrMore(Box<Pattern>),
    ZeroOrOne(Box<Pattern>),
    Group(Vec<Pattern>),
    Alternation(Vec<Pattern>, Vec<Pattern>),
    Backreference(usize),
}

impl Pattern {
    pub fn is_equivalent(&self, other: &Pattern) -> bool {
        match (self, other) {
            (Pattern::StartAnchor, Pattern::StartAnchor) => true,
            (Pattern::EndAnchor, Pattern::EndAnchor) => true,
            (Pattern::Digit, Pattern::Digit) => true,
            (Pattern::Word, Pattern::Word) => true,
            (Pattern::Wildcard, Pattern::Wildcard) => true,
            (Pattern::Lit(c1), Pattern::Lit(c2)) => c1 == c2,
            (Pattern::CharacterClass { set: s1, neg: n1 }, Pattern::CharacterClass { set: s2, neg: n2 }) => {
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
            (Pattern::Group(patterns1), Pattern::Group(patterns2)) => {
                patterns1.len() == patterns2.len() &&
                patterns1.iter().zip(patterns2.iter()).all(|(p1, p2)| p1.is_equivalent(p2))
            },
            (Pattern::Alternation(left1, right1), Pattern::Alternation(left2, right2)) => {
                left1.len() == left2.len() && right1.len() == right2.len() &&
                left1.iter().zip(left2.iter()).all(|(p1, p2)| p1.is_equivalent(p2)) &&
                right1.iter().zip(right2.iter()).all(|(p1, p2)| p1.is_equivalent(p2))
            },
            (Pattern::Backreference(n1), Pattern::Backreference(n2)) => n1 == n2,
            _ => false,
        }
    }
}

pub struct Parser;

#[derive(Debug)]
pub enum ParseError {
    DanglingBackslash,
    DanglingPlus,
    UnclosedCharacterClass,
    UnclosedGroup,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::DanglingBackslash => write!(f, "dangling backslash in pattern"),
            ParseError::DanglingPlus => write!(f, "dangling '+' operator"),
            ParseError::UnclosedCharacterClass => write!(f, "unclosed character class"),
            ParseError::UnclosedGroup => write!(f, "unclosed group"),
        }
    }
}

impl Parser {
    pub fn parse(pattern: &str) -> Result<Vec<Pattern>, ParseError> {
        if pattern.contains('|') && !Self::is_inside_group(pattern) {
            return Self::parse_top_level_alternation(pattern);
        }

        let mut chars = pattern.chars().peekable();
        let mut patterns = Vec::new();

        if chars.peek() == Some(&'^') {
            patterns.push(Pattern::StartAnchor);
            chars.next();
        }

        while let Some(&ch) = chars.peek() {
            let pattern = match ch {
                '\\' => Self::parse_escape(&mut chars)?,
                '[' => Self::parse_character_class(&mut chars)?,
                '$' => Self::parse_dollar(&mut chars),
                '.' => Self::parse_wildcard(&mut chars),
                '(' => Self::parse_group(&mut chars)?,
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

    fn is_inside_group(pattern: &str) -> bool {
        let mut depth = 0;
        for ch in pattern.chars() {
            match ch {
                '(' => depth += 1,
                ')' => depth -= 1,
                '|' if depth == 0 => return false,
                _ => {}
            }
        }
        true
    }

    fn parse_top_level_alternation(pattern: &str) -> Result<Vec<Pattern>, ParseError> {
        if let Some(pipe_pos) = pattern.find('|') {
            let left_part = &pattern[..pipe_pos];
            let right_part = &pattern[pipe_pos + 1..];

            let left_patterns = Self::parse(left_part)?;
            let right_patterns = Self::parse(right_part)?;

            Ok(vec![Pattern::Alternation(left_patterns, right_patterns)])
        } else {
            Self::parse(pattern)
        }
    }

    fn parse_escape(chars: &mut Peekable<std::str::Chars>) -> Result<Pattern, ParseError> {
        chars.next();
        match chars.next() {
            Some('d') => Ok(Pattern::Digit),
            Some('w') => Ok(Pattern::Word),
            Some(c) if c.is_ascii_digit() => {
                let digit = c.to_digit(10).unwrap() as usize;
                Ok(Pattern::Backreference(digit))
            },
            Some(c) => Ok(Pattern::Lit(c)),
            None => Err(ParseError::DanglingBackslash),
        }
    }

    fn parse_character_class(chars: &mut Peekable<std::str::Chars>) -> Result<Pattern, ParseError> {
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
                ']' => return Ok(Pattern::CharacterClass { set, neg: negated }),
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
        
        Err(ParseError::UnclosedCharacterClass)
    }

    fn parse_dollar(chars: &mut Peekable<std::str::Chars>) -> Pattern {
        chars.next();
        if chars.peek().is_some() {
            Pattern::Lit('$')
        } else {
            Pattern::EndAnchor
        }
    }

    fn parse_wildcard(chars: &mut Peekable<std::str::Chars>) -> Pattern {
        chars.next();
        Pattern::Wildcard
    }

    fn parse_literal(chars: &mut Peekable<std::str::Chars>) -> Pattern {
        let ch = chars.next().unwrap();
        Pattern::Lit(ch)
    }

    fn parse_group(chars: &mut Peekable<std::str::Chars>) -> Result<Pattern, ParseError> {
        chars.next();

        let mut group_content = String::new();
        let mut paren_depth = 1;

        while let Some(ch) = chars.next() {
            match ch {
                '(' => {
                    paren_depth += 1;
                    group_content.push(ch);
                },
                ')' => {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        break;
                    } else {
                        group_content.push(ch);
                    }
                },
                c => group_content.push(c),
            }
        }

        if paren_depth > 0 {
            return Err(ParseError::UnclosedGroup);
        }

        if group_content.contains('|') {
            let alternation = Self::parse_alternation(&group_content)?;
            Ok(Pattern::Group(vec![alternation]))
        } else {
            let patterns = Self::parse(&group_content)?;
            Ok(Pattern::Group(patterns))
        }
    }

    fn parse_alternation(content: &str) -> Result<Pattern, ParseError> {
        if let Some(pipe_pos) = content.find('|') {
            let left_part = &content[..pipe_pos];
            let right_part = &content[pipe_pos + 1..];

            let left_patterns = Self::parse(left_part)?;
            let right_patterns = Self::parse(right_part)?;

            Ok(Pattern::Alternation(left_patterns, right_patterns))
        } else {
            let patterns = Self::parse(content)?;
            Ok(Pattern::Group(patterns))
        }
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
    captures: Vec<String>,
}

#[derive(Debug)]
pub struct MatchContext {
    pub input_idx: usize,
    pub pattern_idx: usize,
}

impl<'a> Matcher<'a> {
    pub fn new(input: &'a [char], patterns: &'a [Pattern]) -> Self {
        Self { input, patterns, debug: false, captures: Vec::new() }
    }

    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
    }

    pub fn find_match(&mut self) -> bool {
        if self.patterns.is_empty() {
            return false;
        }

        let start_positions = self.get_start_positions();

        for start_pos in start_positions {
            if self.debug {
                println!("\nAttempting match starting at index {} (char '{}')",
                    start_pos, self.input.get(start_pos).unwrap_or(&' '));
            }

            self.captures.clear();
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

    fn match_from_position(&mut self, start_pos: usize) -> bool {
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

    fn match_pattern(&mut self, input_idx: &mut usize, pattern: &Pattern, remaining_patterns: &[Pattern]) -> bool {
        match pattern {
            Pattern::StartAnchor => *input_idx == 0,
            Pattern::EndAnchor => *input_idx == self.input.len(),
            Pattern::Digit => self.match_digit(input_idx),
            Pattern::Word => self.match_word(input_idx),
            Pattern::Wildcard => self.match_wildcard(input_idx), 
            Pattern::Lit(c) => self.match_literal(input_idx, *c),
            Pattern::CharacterClass { set, neg } => self.match_character_class(input_idx, set, *neg),
            Pattern::OneOrMore(inner) => self.match_one_or_more(input_idx, inner, remaining_patterns),
            Pattern::ZeroOrMore(inner) => self.match_zero_or_more(input_idx, inner, remaining_patterns),
            Pattern::ZeroOrOne(inner) => self.match_zero_or_one(input_idx, inner, remaining_patterns),
            Pattern::Group(patterns) => self.match_group(input_idx, patterns),
            Pattern::Alternation(left, right) => self.match_alternation(input_idx, left, right),
            Pattern::Backreference(n) => self.match_backreference(input_idx, *n),
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

    fn match_wildcard(&self, input_idx: &mut usize) -> bool {
        if *input_idx < self.input.len() {
            *input_idx += 1;
            true
        } else {
            false
        }
    }

    fn match_literal(&self, input_idx: &mut usize, literal: char) -> bool {
        if *input_idx < self.input.len() && self.input[*input_idx] == literal {
            *input_idx += 1;
            true
        } else {
            false
        }
    }

    fn match_character_class(&self, input_idx: &mut usize, set: &HashSet<char>, negated: bool) -> bool {
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

    fn match_one_or_more(&mut self, input_idx: &mut usize, inner: &Pattern, remaining_patterns: &[Pattern]) -> bool {
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

    fn match_zero_or_more(&mut self, input_idx: &mut usize, inner: &Pattern, remaining_patterns: &[Pattern]) -> bool {
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

    fn match_zero_or_one(&mut self, input_idx: &mut usize, inner: &Pattern, remaining_patterns: &[Pattern]) -> bool {
        let original_idx = *input_idx;

        if self.match_pattern(input_idx, inner, &[]) {
            let reserved_count = self.count_equivalent_patterns(inner, remaining_patterns);
            if reserved_count > 0 {
                *input_idx = original_idx;
            }
        }
        
        true
    }

    fn match_group(&mut self, input_idx: &mut usize, patterns: &[Pattern]) -> bool {
        let original_idx = *input_idx;

        for pattern in patterns {
            if !self.match_pattern(input_idx, pattern, &[]) {
                *input_idx = original_idx;
                return false;
            }
        }

        let captured_text: String = self.input[original_idx..*input_idx].iter().collect();
        self.captures.push(captured_text);

        true
    }

    fn match_backreference(&self, input_idx: &mut usize, group_number: usize) -> bool {
        if group_number == 0 || group_number > self.captures.len() {
            return false;
        }

        let captured_text = &self.captures[group_number - 1];
        let captured_chars: Vec<char> = captured_text.chars().collect();

        if *input_idx + captured_chars.len() > self.input.len() {
            return false;
        }

        for (i, &ch) in captured_chars.iter().enumerate() {
            if self.input[*input_idx + i] != ch {
                return false;
            }
        }

        *input_idx += captured_chars.len();
        true
    }

    fn match_alternation(&mut self, input_idx: &mut usize, left: &[Pattern], right: &[Pattern]) -> bool {
        let original_idx = *input_idx;

        let mut temp_idx = *input_idx;
        let mut left_matches = true;
        for pattern in left {
            if !self.match_pattern(&mut temp_idx, pattern, &[]) {
                left_matches = false;
                break;
            }
        }

        if left_matches {
            *input_idx = temp_idx;
            return true;
        }

        *input_idx = original_idx;
        let mut temp_idx = *input_idx;
        let mut right_matches = true;
        for pattern in right {
            if !self.match_pattern(&mut temp_idx, pattern, &[]) {
                right_matches = false;
                break;
            }
        }

        if right_matches {
            *input_idx = temp_idx;
            return true;
        }

        *input_idx = original_idx;
        false
    }

    fn count_equivalent_patterns(&self, target: &Pattern, patterns: &[Pattern]) -> usize {
        let mut count = 0;
        for pattern in patterns {
            match (target, pattern) {
                _ if target.is_equivalent(pattern) => count += 1,
                (Pattern::Wildcard, Pattern::Lit(_)) |
                (Pattern::Wildcard, Pattern::Digit) |
                (Pattern::Wildcard, Pattern::Word) |
                (Pattern::Wildcard, Pattern::CharacterClass { .. }) => count += 1,
                _ => break,
            }
        }
        count
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

        let mut matcher = Matcher::new(&chars, &self.patterns)
            .with_debug(self.debug);
        matcher.find_match()
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
