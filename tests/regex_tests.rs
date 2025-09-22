use codecrafters_grep::parse_regex;

fn test_pattern(pattern: &str, text: &str, expected: bool) {
    let regex = parse_regex(pattern).expect(&format!("Failed to parse pattern: {}", pattern));
    let result = regex.find_match(text);
    assert_eq!(result, expected,
        "Pattern '{}' against text '{}' - expected: {}, got: {}",
        pattern, text, expected, result);
}

#[cfg(test)]
mod basic_character_tests {
    use super::*;

    #[test]
    fn test_char_match() {
        test_pattern("a", "abc", true);
    }

    #[test]
    fn test_char_no_match() {
        test_pattern("x", "abc", false);
    }

    #[test]
    fn test_char_at_end() {
        test_pattern("c", "abc", true);
    }

    #[test]
    fn test_empty_pattern() {
        test_pattern("", "", true);
    }

    #[test]
    fn test_empty_text() {
        test_pattern("a", "", false);
    }
}

#[cfg(test)]
mod dot_wildcard_tests {
    use super::*;

    #[test]
    fn test_dot_match() {
        test_pattern(".", "a", true);
    }

    #[test]
    fn test_dot_sequence() {
        test_pattern("a.c", "abc", true);
    }

    #[test]
    fn test_dot_no_match() {
        test_pattern(".", "", false);
    }

    #[test]
    fn test_multiple_dots() {
        test_pattern("...", "abc", true);
    }

    #[test]
    fn test_multiple_dots_fail() {
        test_pattern("....", "abc", false);
    }
}

#[cfg(test)]
mod anchor_tests {
    use super::*;

    #[test]
    fn test_start_anchor() {
        test_pattern("^abc", "abc123", true);
    }

    #[test]
    fn test_start_anchor_fail() {
        test_pattern("^abc", "123abc", false);
    }

    #[test]
    fn test_end_anchor() {
        test_pattern("abc$", "123abc", true);
    }

    #[test]
    fn test_end_anchor_fail() {
        test_pattern("abc$", "abc123", false);
    }

    #[test]
    fn test_both_anchors() {
        test_pattern("^abc$", "abc", true);
    }

    #[test]
    fn test_both_anchors_fail() {
        test_pattern("^abc$", "abcd", false);
    }
}

#[cfg(test)]
mod character_class_tests {
    use super::*;

    #[test]
    fn test_char_class() {
        test_pattern("[abc]", "banana", true);
    }

    #[test]
    fn test_char_class_no_match() {
        test_pattern("[xyz]", "banana", false);
    }

    #[test]
    fn test_negated_char_class() {
        test_pattern("[^abc]", "def", true);
    }

    #[test]
    fn test_negated_char_class_fail() {
        test_pattern("[^abc]", "abc", false);
    }
}

#[cfg(test)]
mod star_quantifier_tests {
    use super::*;

    #[test]
    fn test_star_zero_matches() {
        test_pattern("a*", "bbb", true);
    }

    #[test]
    fn test_star_one_match() {
        test_pattern("a*", "aaa", true);
    }

    #[test]
    fn test_star_with_char() {
        test_pattern("ab*", "a", true);
    }

    #[test]
    fn test_star_with_char_multiple() {
        test_pattern("ab*", "abb", true);
    }

    #[test]
    fn test_star_greedy() {
        test_pattern("a*b", "aaab", true);
    }

    #[test]
    fn test_star_with_dot() {
        test_pattern(".*", "anything", true);
    }
}

#[cfg(test)]
mod plus_quantifier_tests {
    use super::*;

    #[test]
    fn test_plus_one_match() {
        test_pattern("a+", "aaa", true);
    }

    #[test]
    fn test_plus_zero_fail() {
        test_pattern("a+", "bbb", false);
    }

    #[test]
    fn test_plus_with_char() {
        test_pattern("ab+", "abb", true);
    }

    #[test]
    fn test_plus_fail() {
        test_pattern("ab+", "a", false);
    }

    #[test]
    fn test_plus_greedy() {
        test_pattern("a+b", "aaab", true);
    }
}

#[cfg(test)]
mod question_quantifier_tests {
    use super::*;

    #[test]
    fn test_question_zero() {
        test_pattern("a?", "b", true);
    }

    #[test]
    fn test_question_one() {
        test_pattern("a?", "a", true);
    }

    #[test]
    fn test_question_with_char() {
        test_pattern("ab?", "a", true);
    }

    #[test]
    fn test_question_with_char_both() {
        test_pattern("ab?", "ab", true);
    }

    #[test]
    fn test_question_partial_match() {
        test_pattern("ab?", "ac", true);
    }
}

#[cfg(test)]
mod sequence_tests {
    use super::*;

    #[test]
    fn test_simple_sequence() {
        test_pattern("abc", "abc", true);
    }

    #[test]
    fn test_sequence_in_text() {
        test_pattern("abc", "xabcy", true);
    }

    #[test]
    fn test_sequence_fail() {
        test_pattern("abc", "axc", false);
    }

    #[test]
    fn test_long_sequence() {
        test_pattern("hello", "hello world", true);
    }

    #[test]
    fn test_sequence_partial() {
        test_pattern("hello", "hell", false);
    }
}

#[cfg(test)]
mod alternation_tests {
    use super::*;

    #[test]
    fn test_alternation_left() {
        test_pattern("cat|dog", "cat", true);
    }

    #[test]
    fn test_alternation_right() {
        test_pattern("cat|dog", "dog", true);
    }

    #[test]
    fn test_alternation_fail() {
        test_pattern("cat|dog", "bird", false);
    }

    #[test]
    fn test_alternation_in_text() {
        test_pattern("cat|dog", "I have a cat", true);
    }

    #[test]
    fn test_multiple_alternation() {
        test_pattern("a|b|c", "banana", true);
    }
}

#[cfg(test)]
mod complex_mixed_patterns {
    use super::*;

    #[test]
    fn test_sequence_with_star() {
        test_pattern("ab*c", "ac", true);
    }

    #[test]
    fn test_sequence_with_star_multiple() {
        test_pattern("ab*c", "abc", true);
    }

    #[test]
    fn test_sequence_with_star_many() {
        test_pattern("ab*c", "abbc", true);
    }

    #[test]
    fn test_alternation_with_quantifier() {
        test_pattern("(cat|dog)+", "catdog", true);
    }

    #[test]
    fn test_alternation_with_quantifier_reverse() {
        test_pattern("(cat|dog)+", "dogcat", true);
    }

    #[test]
    fn test_dots_with_quantifiers() {
        test_pattern("a.+b", "aXXXb", true);
    }

    #[test]
    fn test_complex_pattern() {
        test_pattern("^(hello|hi).* world$", "hello cruel world", true);
    }

    #[test]
    fn test_complex_pattern_fail() {
        test_pattern("^(hello|hi).* world$", "goodbye world", false);
    }
}

#[cfg(test)]
mod nested_patterns {
    use super::*;

    #[test]
    fn test_nested_groups() {
        test_pattern("(a(b|c))+", "abac", true);
    }

    #[test]
    fn test_nested_quantifiers() {
        test_pattern("(a+b*)+", "aaabbbaab", true);
    }

    #[test]
    fn test_complex_nesting() {
        test_pattern("^((a|b)+c)*$", "acbcac", true);
    }

    #[test]
    fn test_anchor_with_groups() {
        test_pattern("^(test|demo)$", "test", true);
    }

    #[test]
    fn test_anchor_with_groups_fail() {
        test_pattern("^(test|demo)$", "testing", false);
    }
}

#[cfg(test)]
mod edge_cases {
    use super::*;

    #[test]
    fn test_star_of_star() {
        test_pattern("(a*)*", "aaa", true);
    }

    #[test]
    fn test_plus_of_question() {
        test_pattern("(a?)+", "aaa", true);
    }

    #[test]
    fn test_multiple_dots() {
        test_pattern("...+", "abcd", true);
    }

    #[test]
    fn test_anchors_only() {
        test_pattern("^$", "", true);
    }

    #[test]
    fn test_anchors_only_fail() {
        test_pattern("^$", "a", false);
    }
}

#[cfg(test)]
mod real_world_patterns {
    use super::*;

    #[test]
    fn test_digit_pattern() {
        test_pattern("\\d+", "abc123def", true);
    }

    #[test]
    fn test_word_pattern() {
        test_pattern("\\w+", "hello_world", true);
    }

    #[test]
    fn test_digit_pattern_fail() {
        test_pattern("\\d+", "abcdef", false);
    }

    #[test]
    fn test_email_like() {
        test_pattern(".+@.+", "user@domain.com", true);
    }

    #[test]
    fn test_phone_like() {
        test_pattern("\\d+", "123-456-7890", true);
    }

    #[test]
    fn test_url_like() {
        test_pattern("http.+", "https://example.com", true);
    }
}

#[cfg(test)]
mod stress_tests {
    use super::*;

    #[test]
    fn test_long_pattern() {
        test_pattern("a+b+c+", "aaabbbccc", true);
    }

    #[test]
    fn test_alternation_chain() {
        test_pattern("a|b|c|d|e", "elephant", true);
    }

    #[test]
    fn test_deep_nesting() {
        test_pattern("((a|b)+c)*", "abcabcabc", true);
    }

    #[test]
    fn test_many_quantifiers() {
        test_pattern("a*b*c*d*", "abcd", true);
    }
}