use codecrafters_grep::parse_regex;

fn test_pattern(pattern: &str, text: &str, expected: bool) {
    let regex = parse_regex(pattern).expect(&format!("Failed to parse pattern: {}", pattern));
    let result = regex.find_match(text);
    assert_eq!(result, expected,
        "Pattern '{}' against text '{}' - expected: {}, got: {}",
        pattern, text, expected, result);
}

#[cfg(test)]
mod basic_group_tests {
    use super::*;

    #[test]
    fn test_simple_group() {
        test_pattern("(abc)", "abc", true);
    }

    #[test]
    fn test_group_in_sequence() {
        test_pattern("x(abc)y", "xabcy", true);
    }

    #[test]
    fn test_group_fail() {
        test_pattern("(abc)", "def", false);
    }

    #[test]
    fn test_empty_group() {
        test_pattern("()", "hello", true);
    }

    #[test]
    fn test_multiple_groups() {
        test_pattern("(a)(b)", "ab", true);
    }

    #[test]
    fn test_multiple_groups_fail() {
        test_pattern("(a)(b)", "ac", false);
    }
}

#[cfg(test)]
mod nested_group_tests {
    use super::*;

    #[test]
    fn test_nested_groups() {
        test_pattern("(a(b)c)", "abc", true);
    }

    #[test]
    fn test_nested_groups_multiple() {
        test_pattern("((a)b)", "ab", true);
    }

    #[test]
    fn test_deep_nesting() {
        test_pattern("(((a)))", "a", true);
    }

    #[test]
    fn test_nested_with_alternation() {
        test_pattern("(a(b|c)d)", "abd", true);
    }

    #[test]
    fn test_nested_with_alternation_second() {
        test_pattern("(a(b|c)d)", "acd", true);
    }
}

#[cfg(test)]
mod group_with_quantifiers_tests {
    use super::*;

    #[test]
    fn test_group_with_star() {
        test_pattern("(ab)*", "", true);
    }

    #[test]
    fn test_group_with_star_once() {
        test_pattern("(ab)*", "ab", true);
    }

    #[test]
    fn test_group_with_star_multiple() {
        test_pattern("(ab)*", "abab", true);
    }

    #[test]
    fn test_group_with_plus() {
        test_pattern("(ab)+", "ab", true);
    }

    #[test]
    fn test_group_with_plus_multiple() {
        test_pattern("(ab)+", "ababab", true);
    }

    #[test]
    fn test_group_with_plus_fail() {
        test_pattern("(ab)+", "", false);
    }

    #[test]
    fn test_group_with_question() {
        test_pattern("(ab)?", "", true);
    }

    #[test]
    fn test_group_with_question_once() {
        test_pattern("(ab)?", "ab", true);
    }
}

#[cfg(test)]
mod basic_backreference_tests {
    use super::*;

    #[test]
    fn test_simple_backreference() {
        test_pattern("(a)\\1", "aa", true);
    }

    #[test]
    fn test_simple_backreference_fail() {
        test_pattern("(a)\\1", "ab", false);
    }

    #[test]
    fn test_longer_backreference() {
        test_pattern("(abc)\\1", "abcabc", true);
    }

    #[test]
    fn test_longer_backreference_fail() {
        test_pattern("(abc)\\1", "abcdef", false);
    }

    #[test]
    fn test_backreference_with_alternation() {
        test_pattern("(a|b)\\1", "aa", true);
    }

    #[test]
    fn test_backreference_with_alternation_second() {
        test_pattern("(a|b)\\1", "bb", true);
    }

    #[test]
    fn test_backreference_with_alternation_fail() {
        test_pattern("(a|b)\\1", "ab", false);
    }
}

#[cfg(test)]
mod multiple_backreference_tests {
    use super::*;

    #[test]
    fn test_two_groups_two_backreferences() {
        test_pattern("(a)(b)\\1\\2", "abab", true);
    }

    #[test]
    fn test_two_groups_two_backreferences_fail() {
        test_pattern("(a)(b)\\1\\2", "abba", false);
    }

    #[test]
    fn test_reversed_backreferences() {
        test_pattern("(a)(b)\\2\\1", "abba", true);
    }

    #[test]
    fn test_multiple_same_backreference() {
        test_pattern("(a)\\1\\1", "aaa", true);
    }

    #[test]
    fn test_multiple_same_backreference_fail() {
        test_pattern("(a)\\1\\1", "aab", false);
    }
}

#[cfg(test)]
mod complex_pattern_tests {
    use super::*;

    #[test]
    fn test_palindrome_like() {
        test_pattern("^(.).*\\1$", "aba", true);
    }

    #[test]
    fn test_palindrome_like_longer() {
        test_pattern("^(.).*\\1$", "abcba", true);
    }

    #[test]
    fn test_palindrome_like_fail() {
        test_pattern("^(.).*\\1$", "abc", false);
    }

    #[test]
    fn test_doubled_pattern() {
        test_pattern("^(.+)\\1$", "abcabc", true);
    }

    #[test]
    fn test_doubled_pattern_fail() {
        test_pattern("^(.+)\\1$", "abcdef", false);
    }

    #[test]
    fn test_word_boundary_like() {
        test_pattern("(\\w+) \\1", "hello hello", true);
    }

    #[test]
    fn test_word_boundary_like_fail() {
        test_pattern("(\\w+) \\1", "hello world", false);
    }
}

#[cfg(test)]
mod advanced_group_tests {
    use super::*;

    #[test]
    fn test_alternation_in_group() {
        test_pattern("(cat|dog)", "cat", true);
    }

    #[test]
    fn test_alternation_in_group_second() {
        test_pattern("(cat|dog)", "dog", true);
    }

    #[test]
    fn test_alternation_in_group_fail() {
        test_pattern("(cat|dog)", "bird", false);
    }

    #[test]
    fn test_quantified_alternation_group() {
        test_pattern("(a|b)+", "ababab", true);
    }

    #[test]
    fn test_quantified_alternation_group_single() {
        test_pattern("(a|b)+", "a", true);
    }

    #[test]
    fn test_quantified_alternation_group_fail() {
        test_pattern("(a|b)+", "c", false);
    }
}

#[cfg(test)]
mod edge_case_tests {
    use super::*;

    #[test]
    fn test_group_with_dot() {
        test_pattern("(.)\\1", "aa", true);
    }

    #[test]
    fn test_group_with_dot_different_chars() {
        test_pattern("(.)\\1", "ab", false);
    }

    #[test]
    fn test_group_with_character_class() {
        test_pattern("([abc])\\1", "aa", true);
    }

    #[test]
    fn test_group_with_character_class_fail() {
        test_pattern("([abc])\\1", "ab", false);
    }

    #[test]
    fn test_optional_group_with_backreference() {
        test_pattern("(a)?\\1", "", true);
    }

    #[test]
    fn test_optional_group_with_backreference_present() {
        test_pattern("(a)?\\1", "aa", true);
    }

    #[test]
    fn test_star_group_with_backreference() {
        test_pattern("(a)*\\1", "", true);
    }

    #[test]
    fn test_star_group_with_backreference_once() {
        test_pattern("(a)*\\1", "aa", true);
    }
}

#[cfg(test)]
mod real_world_patterns {
    use super::*;

    #[test]
    fn test_html_tag_matching() {
        test_pattern("<(\\w+)>.*</\\1>", "<div>content</div>", true);
    }

    #[test]
    fn test_html_tag_matching_fail() {
        test_pattern("<(\\w+)>.*</\\1>", "<div>content</span>", false);
    }

    #[test]
    fn test_quoted_string() {
        test_pattern("([\"']).*\\1", "\"hello\"", true);
    }

    #[test]
    fn test_quoted_string_single() {
        test_pattern("([\"']).*\\1", "'hello'", true);
    }

    #[test]
    fn test_quoted_string_fail() {
        test_pattern("([\"']).*\\1", "\"hello'", false);
    }
}