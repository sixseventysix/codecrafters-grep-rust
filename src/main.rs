use std::collections::HashSet;
use std::env;
use std::io;
use std::iter::Peekable;
use std::process;

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
    Group(Vec<Pattern>, usize),
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
            (
                Pattern::CharacterClass { set: s1, neg: n1 },
                Pattern::CharacterClass { set: s2, neg: n2 },
            ) => s1 == s2 && n1 == n2,
            (Pattern::OneOrMore(inner1), Pattern::OneOrMore(inner2)) => {
                inner1.is_equivalent(inner2)
            }
            (Pattern::ZeroOrMore(inner1), Pattern::ZeroOrMore(inner2)) => {
                inner1.is_equivalent(inner2)
            }
            (Pattern::ZeroOrOne(inner1), Pattern::ZeroOrOne(inner2)) => {
                inner1.is_equivalent(inner2)
            }
            (Pattern::Group(patterns1, num1), Pattern::Group(patterns2, num2)) => {
                num1 == num2
                    && patterns1.len() == patterns2.len()
                    && patterns1
                        .iter()
                        .zip(patterns2.iter())
                        .all(|(p1, p2)| p1.is_equivalent(p2))
            }
            (Pattern::Alternation(left1, right1), Pattern::Alternation(left2, right2)) => {
                left1.len() == left2.len()
                    && right1.len() == right2.len()
                    && left1
                        .iter()
                        .zip(left2.iter())
                        .all(|(p1, p2)| p1.is_equivalent(p2))
                    && right1
                        .iter()
                        .zip(right2.iter())
                        .all(|(p1, p2)| p1.is_equivalent(p2))
            }
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
        let mut counter = 0;
        let mut patterns = Self::parse_with_counter(pattern, &mut counter)?;
        Self::cleanup_end_anchor(&mut patterns);
        Ok(patterns)
    }

    fn parse_with_counter(pattern: &str, counter: &mut usize) -> Result<Vec<Pattern>, ParseError> {
        if pattern.contains('|') && !Self::is_inside_group(pattern) {
            return Self::parse_top_level_alternation_with_counter(pattern, counter);
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
                '(' => Self::parse_group_with_counter(&mut chars, counter)?,
                _ => Self::parse_literal(&mut chars),
            };

            patterns.push(pattern);

            match chars.peek() {
                Some('+') => {
                    chars.next();
                    let last_pattern = patterns.pop().ok_or(ParseError::DanglingPlus)?;
                    patterns.push(Pattern::OneOrMore(Box::new(last_pattern)));
                }
                Some('*') => {
                    chars.next();
                    let last_pattern = patterns.pop().ok_or(ParseError::DanglingPlus)?;
                    patterns.push(Pattern::ZeroOrMore(Box::new(last_pattern)));
                }
                Some('?') => {
                    chars.next();
                    let last_pattern = patterns.pop().ok_or(ParseError::DanglingPlus)?;
                    patterns.push(Pattern::ZeroOrOne(Box::new(last_pattern)));
                }
                _ => {}
            }
        }

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

    fn parse_top_level_alternation_with_counter(
        pattern: &str,
        counter: &mut usize,
    ) -> Result<Vec<Pattern>, ParseError> {
        if let Some(pipe_pos) = pattern.find('|') {
            let left_part = &pattern[..pipe_pos];
            let right_part = &pattern[pipe_pos + 1..];

            let left_patterns = Self::parse_with_counter(left_part, counter)?;
            let right_patterns = Self::parse_with_counter(right_part, counter)?;

            Ok(vec![Pattern::Alternation(left_patterns, right_patterns)])
        } else {
            Self::parse_with_counter(pattern, counter)
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
            }
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
                }
                c => {
                    set.insert(c);
                }
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

    fn parse_group_with_counter(
        chars: &mut Peekable<std::str::Chars>,
        counter: &mut usize,
    ) -> Result<Pattern, ParseError> {
        chars.next();
        *counter += 1;
        let group_number = *counter;

        let mut group_content = String::new();
        let mut paren_depth = 1;

        while let Some(ch) = chars.next() {
            match ch {
                '(' => {
                    paren_depth += 1;
                    group_content.push(ch);
                }
                ')' => {
                    paren_depth -= 1;
                    if paren_depth == 0 {
                        break;
                    } else {
                        group_content.push(ch);
                    }
                }
                c => group_content.push(c),
            }
        }

        if paren_depth > 0 {
            return Err(ParseError::UnclosedGroup);
        }

        if group_content.contains('|') {
            let alternation = Self::parse_alternation_with_counter(&group_content, counter)?;
            Ok(Pattern::Group(vec![alternation], group_number))
        } else {
            let patterns = Self::parse_with_counter(&group_content, counter)?;
            Ok(Pattern::Group(patterns, group_number))
        }
    }

    fn parse_alternation_with_counter(
        content: &str,
        counter: &mut usize,
    ) -> Result<Pattern, ParseError> {
        if let Some(pipe_pos) = content.find('|') {
            let left_part = &content[..pipe_pos];
            let right_part = &content[pipe_pos + 1..];

            let left_patterns = Self::parse_with_counter(left_part, counter)?;
            let right_patterns = Self::parse_with_counter(right_part, counter)?;

            Ok(Pattern::Alternation(left_patterns, right_patterns))
        } else {
            let patterns = Self::parse_with_counter(content, counter)?;
            Ok(Pattern::Group(patterns, 0)) // This shouldn't happen in practice
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
    captures: Vec<Option<String>>,
}

#[derive(Debug)]
pub struct MatchContext {
    pub input_idx: usize,
    pub pattern_idx: usize,
}

impl<'a> Matcher<'a> {
    pub fn new(input: &'a [char], patterns: &'a [Pattern]) -> Self {
        let max_groups = Self::count_groups(patterns);
        Self {
            input,
            patterns,
            debug: false,
            captures: vec![None; max_groups + 1], // +1 because groups are 1-indexed
        }
    }

    fn count_groups(patterns: &[Pattern]) -> usize {
        let mut max_group = 0;
        for pattern in patterns {
            max_group = max_group.max(Self::max_group_number(pattern));
        }
        max_group
    }

    fn max_group_number(pattern: &Pattern) -> usize {
        match pattern {
            Pattern::Group(patterns, group_num) => {
                let inner_max = patterns
                    .iter()
                    .map(Self::max_group_number)
                    .max()
                    .unwrap_or(0);
                (*group_num).max(inner_max)
            }
            Pattern::Alternation(left, right) => {
                let left_max = left.iter().map(Self::max_group_number).max().unwrap_or(0);
                let right_max = right.iter().map(Self::max_group_number).max().unwrap_or(0);
                left_max.max(right_max)
            }
            Pattern::OneOrMore(inner) | Pattern::ZeroOrMore(inner) | Pattern::ZeroOrOne(inner) => {
                Self::max_group_number(inner)
            }
            _ => 0,
        }
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
                println!(
                    "\nAttempting match starting at index {} (char '{}')",
                    start_pos,
                    self.input.get(start_pos).unwrap_or(&' ')
                );
            }

            for capture in &mut self.captures {
                *capture = None;
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

    fn match_from_position(&mut self, start_pos: usize) -> bool {
        if let Some(end_idx) = self.match_sequence(&self.patterns, start_pos) {
            if self.debug {
                println!("  -> Match consumed up to index {}", end_idx);
            }
            true
        } else {
            false
        }
    }

    fn match_single_pattern(&mut self, pattern: &Pattern, input_idx: usize) -> Option<usize> {
        match pattern {
            Pattern::Digit => self.match_digit(input_idx),
            Pattern::Word => self.match_word(input_idx),
            Pattern::Wildcard => self.match_wildcard(input_idx),
            Pattern::Lit(c) => self.match_literal(input_idx, *c),
            Pattern::CharacterClass { set, neg } => self.match_character_class(input_idx, set, *neg),
            Pattern::Backreference(n) => self.match_backreference(input_idx, *n),
            Pattern::StartAnchor => if input_idx == 0 { Some(input_idx) } else { None },
            Pattern::EndAnchor => if input_idx == self.input.len() { Some(input_idx) } else { None },
            _ => None,
        }
    }

    fn match_sequence(&mut self, patterns: &[Pattern], mut input_idx: usize) -> Option<usize> {
        if patterns.is_empty() {
            return Some(input_idx);
        }

        let pattern = &patterns[0];
        let remaining_patterns = &patterns[1..];

        match pattern {
            Pattern::OneOrMore(inner) => {
                let mut checkpoints = Vec::new();
                let mut current_idx = input_idx;

                loop {
                    let pre_iteration_snapshot = self.snapshot_captures();
                    match self.match_sequence(std::slice::from_ref(inner), current_idx) {
                        Some(next_idx) if next_idx > current_idx => {
                            checkpoints.push((next_idx, self.snapshot_captures()));
                            current_idx = next_idx;
                        }
                        _ => {
                            self.restore_captures(pre_iteration_snapshot);
                            break;
                        }
                    }
                }

                while let Some((candidate_idx, snapshot)) = checkpoints.pop() {
                    self.restore_captures(snapshot);
                    if let Some(result_idx) = self.match_sequence(remaining_patterns, candidate_idx) {
                        return Some(result_idx);
                    }
                }
                None
            }

            Pattern::ZeroOrMore(inner) => {
                let mut checkpoints = vec![(input_idx, self.snapshot_captures())];
                let mut current_idx = input_idx;

                loop {
                    let pre_iteration_snapshot = self.snapshot_captures();
                    match self.match_sequence(std::slice::from_ref(inner), current_idx) {
                        Some(next_idx) if next_idx > current_idx => {
                            checkpoints.push((next_idx, self.snapshot_captures()));
                            current_idx = next_idx;
                        }
                        _ => {
                            self.restore_captures(pre_iteration_snapshot);
                            break;
                        }
                    }
                }

                // Backtrack from longest to shortest
                while let Some((candidate_idx, snapshot)) = checkpoints.pop() {
                    self.restore_captures(snapshot);
                    if let Some(result_idx) = self.match_sequence(remaining_patterns, candidate_idx) {
                        return Some(result_idx);
                    }
                }
                None
            }

            Pattern::ZeroOrOne(inner) => {
                let original_snapshot = self.snapshot_captures();

                if let Some(next_idx) = self.match_sequence(std::slice::from_ref(inner), input_idx) {
                    if let Some(result_idx) = self.match_sequence(remaining_patterns, next_idx) {
                        return Some(result_idx);
                    }
                }
                
                self.restore_captures(original_snapshot);
                self.match_sequence(remaining_patterns, input_idx)
            }

            Pattern::Group(group_patterns, group_num) => {
                let original_snapshot = self.snapshot_captures();
                if let Some(end_idx) = self.match_sequence(group_patterns, input_idx) {
                    let captured_text: String = self.input[input_idx..end_idx].iter().collect();
                    if *group_num > 0 && *group_num < self.captures.len() {
                        self.captures[*group_num] = Some(captured_text);
                    }

                    if let Some(result_idx) = self.match_sequence(remaining_patterns, end_idx) {
                        return Some(result_idx);
                    }
                }

                self.restore_captures(original_snapshot);
                None
            }

            Pattern::Alternation(left, right) => {
                let original_snapshot = self.snapshot_captures();

                if let Some(next_idx) = self.match_sequence(left, input_idx) {
                    if let Some(result_idx) = self.match_sequence(remaining_patterns, next_idx) {
                        return Some(result_idx);
                    }
                }
                
                self.restore_captures(original_snapshot);

                if let Some(next_idx) = self.match_sequence(right, input_idx) {
                    if let Some(result_idx) = self.match_sequence(remaining_patterns, next_idx) {
                        return Some(result_idx);
                    }
                }

                None
            }

            _ => {
                if let Some(next_idx) = self.match_single_pattern(pattern, input_idx) {
                    self.match_sequence(remaining_patterns, next_idx)
                } else {
                    None
                }
            }
        }
    }

    fn match_digit(&self, input_idx: usize) -> Option<usize> {
        if input_idx < self.input.len() && self.input[input_idx].is_ascii_digit() {
            Some(input_idx + 1)
        } else {
            None
        }
    }

    fn match_word(&self, input_idx: usize) -> Option<usize> {
        if input_idx < self.input.len() {
            let ch = self.input[input_idx];
            if ch.is_alphanumeric() || ch == '_' {
                return Some(input_idx + 1);
            }
        }
        None
    }

    fn match_wildcard(&self, input_idx: usize) -> Option<usize> {
        if input_idx < self.input.len() {
            Some(input_idx + 1)
        } else {
            None
        }
    }

    fn match_literal(&self, input_idx: usize, literal: char) -> Option<usize> {
        if input_idx < self.input.len() && self.input[input_idx] == literal {
            Some(input_idx + 1)
        } else {
            None
        }
    }

    fn match_character_class(
        &self,
        input_idx: usize,
        set: &HashSet<char>,
        negated: bool,
    ) -> Option<usize> {
        if input_idx < self.input.len() {
            let ch = self.input[input_idx];
            let in_set = set.contains(&ch);
            if (negated && !in_set) || (!negated && in_set) {
                return Some(input_idx + 1);
            }
        }
        None
    }

    fn match_one_or_more(
        &mut self,
        patterns: &[Pattern],
        input_idx: usize,
        pattern_idx: usize,
        inner: &Pattern,
    ) -> Option<usize> {
        let original_snapshot = self.snapshot_captures();
        let mut checkpoints: Vec<(usize, Vec<Option<String>>)> = Vec::new();
        let mut current_idx = input_idx;

        loop {
            let pre_iteration_snapshot = self.snapshot_captures();
            match self.match_sequence(std::slice::from_ref(inner), current_idx) {
                Some(next_idx) if next_idx > current_idx => {
                    checkpoints.push((next_idx, self.snapshot_captures()));
                    current_idx = next_idx;
                }
                Some(_) => {
                    self.restore_captures(pre_iteration_snapshot);
                    break;
                }
                None => {
                    self.restore_captures(pre_iteration_snapshot);
                    break;
                }
            }
        }

        if checkpoints.is_empty() {
            self.restore_captures(original_snapshot);
            return None;
        }

        while let Some((candidate_idx, snapshot)) = checkpoints.pop() {
            self.restore_captures(snapshot);
            if let Some(result_idx) = self.match_sequence(patterns, candidate_idx)
            {
                return Some(result_idx);
            }
        }

        self.restore_captures(original_snapshot);
        None
    }

    fn match_zero_or_more(
        &mut self,
        patterns: &[Pattern],
        input_idx: usize,
        pattern_idx: usize,
        inner: &Pattern,
    ) -> Option<usize> {
        let original_snapshot = self.snapshot_captures();
        let mut checkpoints: Vec<(usize, Vec<Option<String>>)> =
            vec![(input_idx, self.snapshot_captures())];
        let mut current_idx = input_idx;

        loop {
            let pre_iteration_snapshot = self.snapshot_captures();
            match self.match_sequence(std::slice::from_ref(inner), current_idx) {
                Some(next_idx) if next_idx > current_idx => {
                    checkpoints.push((next_idx, self.snapshot_captures()));
                    current_idx = next_idx;
                }
                Some(_) => {
                    self.restore_captures(pre_iteration_snapshot);
                    break;
                }
                None => {
                    self.restore_captures(pre_iteration_snapshot);
                    break;
                }
            }
        }

        while let Some((candidate_idx, snapshot)) = checkpoints.pop() {
            self.restore_captures(snapshot);
            if let Some(result_idx) = self.match_sequence(patterns, candidate_idx)
            {
                return Some(result_idx);
            }
        }

        self.restore_captures(original_snapshot);
        None
    }

    fn match_zero_or_one(
        &mut self,
        patterns: &[Pattern],
        input_idx: usize,
        pattern_idx: usize,
        inner: &Pattern,
    ) -> Option<usize> {
        let original_snapshot = self.snapshot_captures();

        if let Some(next_idx) = self.match_sequence(std::slice::from_ref(inner), input_idx) {
            if let Some(result_idx) = self.match_sequence(patterns, next_idx) {
                return Some(result_idx);
            }
        }

        self.restore_captures(original_snapshot.clone());
        if let Some(result_idx) = self.match_sequence(patterns, input_idx) {
            return Some(result_idx);
        }

        self.restore_captures(original_snapshot);
        None
    }

    fn match_group(
        &mut self,
        patterns: &[Pattern],
        input_idx: usize,
        pattern_idx: usize,
        group_patterns: &[Pattern],
        group_num: usize,
    ) -> Option<usize> {
        let original_snapshot = self.snapshot_captures();
        let original_idx = input_idx;

        if let Some(end_idx) = self.match_sequence(group_patterns, input_idx) {
            let captured_text: String = self.input[original_idx..end_idx].iter().collect();
            if group_num > 0 && group_num < self.captures.len() {
                self.captures[group_num] = Some(captured_text);
            }

            if let Some(result_idx) = self.match_sequence(patterns, end_idx) {
                return Some(result_idx);
            }
        }

        self.restore_captures(original_snapshot);
        None
    }

    fn match_alternation(
        &mut self,
        patterns: &[Pattern],
        input_idx: usize,
        pattern_idx: usize,
        left: &[Pattern],
        right: &[Pattern],
    ) -> Option<usize> {
        let original_snapshot = self.snapshot_captures();

        self.restore_captures(original_snapshot.clone());
        if let Some(next_idx) = self.match_sequence(left, input_idx) {
            if let Some(result_idx) = self.match_sequence(patterns, next_idx) {
                return Some(result_idx);
            }
        }

        self.restore_captures(original_snapshot.clone());
        if let Some(next_idx) = self.match_sequence(right, input_idx) {
            if let Some(result_idx) = self.match_sequence(patterns, next_idx) {
                return Some(result_idx);
            }
        }

        self.restore_captures(original_snapshot);
        None
    }

    fn match_backreference(&self, input_idx: usize, group_number: usize) -> Option<usize> {
        if group_number == 0 || group_number >= self.captures.len() {
            return None;
        }

        let captured_text = match &self.captures[group_number] {
            Some(text) => text,
            None => return None,
        };

        let captured_chars: Vec<char> = captured_text.chars().collect();

        if input_idx + captured_chars.len() > self.input.len() {
            return None;
        }

        for (i, &ch) in captured_chars.iter().enumerate() {
            if self.input[input_idx + i] != ch {
                return None;
            }
        }

        Some(input_idx + captured_chars.len())
    }

    fn snapshot_captures(&self) -> Vec<Option<String>> {
        self.captures.clone()
    }

    fn restore_captures(&mut self, snapshot: Vec<Option<String>>) {
        self.captures = snapshot;
    }
}

pub struct RegexEngine {
    patterns: Vec<Pattern>,
    debug: bool,
}

impl RegexEngine {
    pub fn new(pattern_str: &str) -> Result<Self, ParseError> {
        let patterns = Parser::parse(pattern_str)?;
        Ok(Self {
            patterns,
            debug: false,
        })
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

        let mut matcher = Matcher::new(&chars, &self.patterns).with_debug(self.debug);
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
