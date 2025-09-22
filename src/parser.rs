use nom::{
    branch::alt,
    character::complete::{anychar, char, none_of},
    combinator::{map, opt, value},
    multi::{many0, many1},
    sequence::preceded,
    IResult,
};
use anyhow::Result;
use crate::regex::Regex;
use std::cell::RefCell;

struct ParseContext {
    group_counter: RefCell<usize>,
}

impl ParseContext {
    fn new() -> Self {
        Self {
            group_counter: RefCell::new(0),
        }
    }

    fn next_group_number(&self) -> usize {
        let mut counter = self.group_counter.borrow_mut();
        *counter += 1;
        *counter
    }
}

pub fn parse_regex(input: &str) -> Result<Regex> {
    let context = ParseContext::new();
    match regex(input, &context) {
        Ok(("", result)) => {
            Ok(result)
        },
        Ok((remaining, _)) => {
            anyhow::bail!("Unexpected input: {}", remaining)
        },
        Err(e) => {
            anyhow::bail!("Parse error: {}", e)
        },
    }
}

fn regex<'a>(input: &'a str, context: &ParseContext) -> IResult<&'a str, Regex> {
    alternation(input, context)
}

fn alternation<'a>(input: &'a str, context: &ParseContext) -> IResult<&'a str, Regex> {
    let (input, first) = sequence(input, context)?;
    let (input, rest) = many0(preceded(char('|'), |i| sequence(i, context)))(input)?;

    Ok((input, rest.into_iter().fold(first, |acc, r| {
        Regex::Alternation(Box::new(acc), Box::new(r))
    })))
}

fn sequence<'a>(input: &'a str, context: &ParseContext) -> IResult<&'a str, Regex> {
    let (input, parts) = many0(|i| quantified(i, context))(input)?;

    let result = if parts.is_empty() {
        Regex::Empty
    } else if parts.len() == 1 {
        parts.into_iter().next().unwrap()
    } else {
        Regex::Sequence(parts)
    };

    Ok((input, result))
}

fn quantified<'a>(input: &'a str, context: &ParseContext) -> IResult<&'a str, Regex> {
    let (input, base) = atom(input, context)?;
    let (input, quantifier) = opt(alt((char('*'), char('+'), char('?'))))(input)?;

    let result = match quantifier {
        Some('*') => Regex::Star(Box::new(base)),
        Some('+') => Regex::Plus(Box::new(base)),
        Some('?') => Regex::Question(Box::new(base)),
        _ => base,
    };

    Ok((input, result))
}

fn atom<'a>(input: &'a str, context: &ParseContext) -> IResult<&'a str, Regex> {
    alt((
        anchor,
        dot,
        char_class,
        escaped_char,
        |i| grouped(i, context),
        literal_char,
    ))(input)
}

fn anchor(input: &str) -> IResult<&str, Regex> {
    alt((
        value(Regex::StartAnchor, char('^')),
        value(Regex::EndAnchor, char('$')),
    ))(input)
}

fn dot(input: &str) -> IResult<&str, Regex> {
    value(Regex::Dot, char('.'))(input)
}

fn char_class(input: &str) -> IResult<&str, Regex> {
    let (input, _) = char('[')(input)?;
    let (input, negated) = opt(char('^'))(input)?;
    let (input, chars) = many1(class_char)(input)?;
    let (input, _) = char(']')(input)?;

    Ok((input, Regex::CharClass(chars, negated.is_some())))
}

fn class_char(input: &str) -> IResult<&str, char> {
    alt((
        preceded(char('\\'), anychar),
        none_of("]"),
    ))(input)
}

fn escaped_char(input: &str) -> IResult<&str, Regex> {
    let (input, _) = char('\\')(input)?;
    let (input, ch) = anychar(input)?;

    let result = match ch {
        'd' => Regex::CharClass("0123456789".chars().collect(), false),
        'w' => Regex::CharClass("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_".chars().collect(), false),
        c if c.is_ascii_digit() => {
            let digit = c.to_digit(10).unwrap() as usize;
            Regex::Backreference(digit)
        }
        c => Regex::Char(c),
    };

    Ok((input, result))
}

fn grouped<'a>(input: &'a str, context: &ParseContext) -> IResult<&'a str, Regex> {
    let (input, _) = char('(')(input)?;
    let group_num = context.next_group_number();
    let (input, inner) = regex(input, context)?;
    let (input, _) = char(')')(input)?;
    Ok((input, Regex::Group(Box::new(inner), group_num)))
}

fn literal_char(input: &str) -> IResult<&str, Regex> {
    map(none_of("^$.*+?()[]|\\"), Regex::Char)(input)
}