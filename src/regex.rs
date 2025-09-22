#[derive(Debug, Clone, PartialEq)]
pub enum Regex {
    Empty,                  // matches empty string
    Char(char),
    Dot,                    // .
    Star(Box<Regex>),       // *
    Plus(Box<Regex>),       // +
    Question(Box<Regex>),   // ?
    Sequence(Vec<Regex>),
    Alternation(Box<Regex>, Box<Regex>), // |
    StartAnchor,            // ^
    EndAnchor,              // $
    CharClass(Vec<char>, bool), // [abc] or [^abc]
    Group(Box<Regex>, usize), // (pattern), group_number
    Backreference(usize),   // \1, \2, etc.
}

impl Regex {
    fn matches_at(&self, text: &[char], pos: usize) -> Option<usize> {
        match self {
            Regex::Empty => Some(pos),
            Regex::Char(c) => {
                if pos < text.len() && text[pos] == *c {
                    Some(pos + 1)
                } else {
                    None
                }
            }
            Regex::Dot => {
                if pos < text.len() {
                    Some(pos + 1)
                } else {
                    None
                }
            }
            Regex::Sequence(regexes) => {
                self.match_sequence_at(text, pos, regexes, 0)
            }
            Regex::Star(inner) => {
                let mut positions = vec![pos];
                let mut current_pos = pos;

                while let Some(new_pos) = inner.matches_at(text, current_pos) {
                    if new_pos == current_pos {
                        break;
                    }
                    current_pos = new_pos;
                    positions.push(current_pos);
                }

                positions.into_iter().rev().next()
            }
            Regex::Plus(inner) => {
                if let Some(first_match) = inner.matches_at(text, pos) {
                    Regex::Star(inner.clone()).matches_at(text, first_match)
                } else {
                    None
                }
            }
            Regex::Question(inner) => {
                inner.matches_at(text, pos).or(Some(pos))
            }
            Regex::Alternation(left, right) => {
                left.matches_at(text, pos).or_else(|| right.matches_at(text, pos))
            }
            Regex::StartAnchor => {
                if pos == 0 {
                    Some(pos)
                } else {
                    None
                }
            }
            Regex::EndAnchor => {
                if pos == text.len() {
                    Some(pos)
                } else {
                    None
                }
            }
            Regex::CharClass(chars, negated) => {
                if pos < text.len() {
                    let ch = text[pos];
                    let in_class = chars.contains(&ch);
                    if (*negated && !in_class) || (!*negated && in_class) {
                        Some(pos + 1)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Regex::Group(inner, _group_num) => {
                inner.matches_at(text, pos)
            }
            Regex::Backreference(_group_num) => {
                None
            }
        }
    }

    pub fn find_match(&self, text: &str) -> bool {
        let chars: Vec<char> = text.chars().collect();
        let max_groups = self.count_groups();
        let mut captures = vec![None; max_groups + 1];

        if let Regex::Sequence(seq) = self {
            if let Some(Regex::StartAnchor) = seq.first() {
                return self.matches_at_with_captures(&chars, 0, &mut captures).is_some();
            }
        }

        for i in 0..=chars.len() {
            for capture in &mut captures {
                *capture = None;
            }
            if self.matches_at_with_captures(&chars, i, &mut captures).is_some() {
                return true;
            }
        }
        false
    }

    fn count_groups(&self) -> usize {
        match self {
            Regex::Group(inner, group_num) => {
                (*group_num).max(inner.count_groups())
            }
            Regex::Sequence(regexes) => {
                regexes.iter().map(|r| r.count_groups()).max().unwrap_or(0)
            }
            Regex::Alternation(left, right) => {
                left.count_groups().max(right.count_groups())
            }
            Regex::Star(inner) | Regex::Plus(inner) | Regex::Question(inner) => {
                inner.count_groups()
            }
            _ => 0,
        }
    }

    fn matches_at_with_captures(&self, text: &[char], pos: usize, captures: &mut Vec<Option<String>>) -> Option<usize> {
        match self {
            Regex::Empty => Some(pos),
            Regex::Char(c) => {
                if pos < text.len() && text[pos] == *c {
                    Some(pos + 1)
                } else {
                    None
                }
            }
            Regex::Dot => {
                if pos < text.len() {
                    Some(pos + 1)
                } else {
                    None
                }
            }
            Regex::Sequence(regexes) => {
                self.match_sequence_with_captures(text, pos, regexes, 0, captures)
            }
            Regex::Star(_) | Regex::Plus(_) | Regex::Question(_) => {
                self.matches_at(text, pos)
            }
            Regex::Alternation(left, right) => {
                let saved_captures = captures.clone();
                if let Some(result) = left.matches_at_with_captures(text, pos, captures) {
                    Some(result)
                } else {
                    *captures = saved_captures;
                    right.matches_at_with_captures(text, pos, captures)
                }
            }
            Regex::StartAnchor => {
                if pos == 0 {
                    Some(pos)
                } else {
                    None
                }
            }
            Regex::EndAnchor => {
                if pos == text.len() {
                    Some(pos)
                } else {
                    None
                }
            }
            Regex::CharClass(chars, negated) => {
                if pos < text.len() {
                    let ch = text[pos];
                    let in_class = chars.contains(&ch);
                    if (*negated && !in_class) || (!*negated && in_class) {
                        Some(pos + 1)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            Regex::Group(inner, group_num) => {
                let start_pos = pos;
                if let Some(end_pos) = inner.matches_at_with_captures(text, pos, captures) {
                    let captured_text: String = text[start_pos..end_pos].iter().collect();
                    if *group_num > 0 && *group_num < captures.len() {
                        captures[*group_num] = Some(captured_text);
                    }
                    Some(end_pos)
                } else {
                    None
                }
            }
            Regex::Backreference(group_num) => {
                if *group_num > 0 && *group_num < captures.len() {
                    if let Some(ref captured_text) = captures[*group_num] {
                        let captured_chars: Vec<char> = captured_text.chars().collect();
                        if pos + captured_chars.len() <= text.len() {
                            for (i, &ch) in captured_chars.iter().enumerate() {
                                if text[pos + i] != ch {
                                    return None;
                                }
                            }
                            Some(pos + captured_chars.len())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    fn match_sequence_with_captures(&self, text: &[char], pos: usize, regexes: &[Regex], index: usize, captures: &mut Vec<Option<String>>) -> Option<usize> {
        if index >= regexes.len() {
            return Some(pos);
        }

        let current_regex = &regexes[index];

        match current_regex {
            Regex::Star(inner) => {
                let mut positions = vec![pos];
                let mut current_pos = pos;

                while let Some(new_pos) = inner.matches_at_with_captures(text, current_pos, captures) {
                    if new_pos == current_pos {
                        break;
                    }
                    current_pos = new_pos;
                    positions.push(current_pos);
                }

                for &try_pos in positions.iter().rev() {
                    let saved_captures = captures.clone();
                    if let Some(result) = self.match_sequence_with_captures(text, try_pos, regexes, index + 1, captures) {
                        return Some(result);
                    }
                    *captures = saved_captures;
                }
                None
            }
            Regex::Plus(inner) => {
                let mut positions = Vec::new();
                let mut current_pos = pos;

                while let Some(new_pos) = inner.matches_at_with_captures(text, current_pos, captures) {
                    if new_pos == current_pos {
                        break;
                    }
                    current_pos = new_pos;
                    positions.push(current_pos);
                }

                if positions.is_empty() {
                    return None;
                }

                for &try_pos in positions.iter().rev() {
                    let saved_captures = captures.clone();
                    if let Some(result) = self.match_sequence_with_captures(text, try_pos, regexes, index + 1, captures) {
                        return Some(result);
                    }
                    *captures = saved_captures;
                }
                None
            }
            Regex::Question(inner) => {
                let saved_captures = captures.clone();
                if let Some(match_pos) = inner.matches_at_with_captures(text, pos, captures) {
                    if let Some(result) = self.match_sequence_with_captures(text, match_pos, regexes, index + 1, captures) {
                        return Some(result);
                    }
                }
                *captures = saved_captures;
                self.match_sequence_with_captures(text, pos, regexes, index + 1, captures)
            }
            _ => {
                if let Some(new_pos) = current_regex.matches_at_with_captures(text, pos, captures) {
                    self.match_sequence_with_captures(text, new_pos, regexes, index + 1, captures)
                } else {
                    None
                }
            }
        }
    }

    fn match_sequence_at(&self, text: &[char], pos: usize, regexes: &[Regex], index: usize) -> Option<usize> {
        if index >= regexes.len() {
            return Some(pos);
        }

        let current_regex = &regexes[index];

        match current_regex {
            Regex::Star(inner) => {
                let mut positions = vec![pos];
                let mut current_pos = pos;

                while let Some(new_pos) = inner.matches_at(text, current_pos) {
                    if new_pos == current_pos {
                        break;
                    }
                    current_pos = new_pos;
                    positions.push(current_pos);
                }

                for &try_pos in positions.iter().rev() {
                    if let Some(result) = self.match_sequence_at(text, try_pos, regexes, index + 1) {
                        return Some(result);
                    }
                }
                None
            }
            Regex::Plus(inner) => {
                let mut positions = Vec::new();
                let mut current_pos = pos;

                while let Some(new_pos) = inner.matches_at(text, current_pos) {
                    if new_pos == current_pos {
                        break;
                    }
                    current_pos = new_pos;
                    positions.push(current_pos);
                }

                if positions.is_empty() {
                    return None;
                }

                for &try_pos in positions.iter().rev() {
                    if let Some(result) = self.match_sequence_at(text, try_pos, regexes, index + 1) {
                        return Some(result);
                    }
                }
                None
            }
            Regex::Question(inner) => {
                if let Some(match_pos) = inner.matches_at(text, pos) {
                    if let Some(result) = self.match_sequence_at(text, match_pos, regexes, index + 1) {
                        return Some(result);
                    }
                }
                self.match_sequence_at(text, pos, regexes, index + 1)
            }
            _ => {
                if let Some(new_pos) = current_regex.matches_at(text, pos) {
                    self.match_sequence_at(text, new_pos, regexes, index + 1)
                } else {
                    None
                }
            }
        }
    }
}