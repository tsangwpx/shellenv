use std::ops::{RangeFrom, RangeTo};

use nom::{
    AsChar, Compare, FindToken, IResult, InputIter, InputLength, InputTake, Offset, Slice,
    branch::alt,
    bytes::complete::{escaped_transform, is_not, tag, take_till},
    character::complete::{alpha1, alphanumeric1, multispace1, none_of, one_of},
    combinator::{map, peek, recognize, value},
    error::ParseError,
    multi::{fold_many0, many0_count},
    sequence::{delimited, pair, preceded, separated_pair, tuple},
};

/*

Parameter expansion in POSIX standards
https://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html

.. in docker
https://docs.docker.com/compose/environment-variables/env-file/

.. in bash
https://manpages.debian.org/bookworm/bash/bash.1.en.html#Parameter_Expansion

.. in dash
https://manpages.debian.org/bookworm/dash/dash.1.en.html#Parameter_Expansion

nom:
https://docs.rs/nom/latest/nom/index.html#modules
*/

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum ExpansionKind<'a> {
    Simple,
    Default(Expr<'a>),     // ${VAR:-default}
    DefaultNull(Expr<'a>), // ${VAR-default}
    Alt(Expr<'a>),         // ${VAR:+alt}
    AltNull(Expr<'a>),     // ${VAR+alt}
    Err(Expr<'a>),         // ${VAR:?error}
    ErrNull(Expr<'a>),     // ${VAR?error}
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) struct Expansion<'a> {
    pub name: &'a str,
    pub kind: ExpansionKind<'a>,
}

impl<'a> Expansion<'a> {
    #[allow(dead_code)]
    pub fn new(name: &'a str, kind: ExpansionKind<'a>) -> Self {
        Self { name, kind }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum Expr<'a> {
    Str(&'a str),
    String(String),
    Substiution(Box<Expansion<'a>>),
    Composite(Vec<Expr<'a>>),
}

impl<'a> Expr<'a> {
    fn is_str_like(&self) -> bool {
        match self {
            Self::Str(_) => true,
            Self::String(_) => true,
            _ => false,
        }
    }
}

fn combine_expr<'a>(left: Expr<'a>, right: Expr<'a>) -> Expr<'a> {
    match (left, right) {
        // return the other if either is empty
        (Expr::Str(""), right) => right,
        (left, Expr::Str("")) => left,

        // impossible case!
        (_, Expr::Composite(_)) => unreachable!(),
        (_, Expr::String(_)) => unreachable!(),

        // construct owned string
        (Expr::Str(left), Expr::Str(right)) => Expr::String([left, right].concat()),

        // expand existing string
        (Expr::String(mut res), Expr::Str(s)) => {
            res.push_str(s);
            Expr::String(res)
        }

        (Expr::Composite(mut items), right) => {
            assert!(items.len() >= 2);

            let last = items.pop().unwrap();
            if last.is_str_like() && right.is_str_like() {
                items.push(combine_expr(last, right));
            } else {
                let x = [last, right];
                items.extend(x);
            }

            Expr::Composite(items)
        }
        (left, right) => Expr::Composite(vec![left, right]),
    }
}

fn parse_identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

/// Parse an escaped character
///
/// control is the escape prefix
/// result is the region of one of escaped character
fn escaped_str<T, S, I, E: ParseError<I>>(
    control: T,
    specials: S,
) -> impl FnMut(I) -> IResult<I, I, E>
where
    I: Clone
        + Offset
        + InputTake
        + Compare<T>
        + InputIter
        + Slice<RangeFrom<usize>>
        + Slice<RangeTo<usize>>,
    <I as InputIter>::Item: AsChar + Copy,
    T: InputLength + Clone,
    S: FindToken<<I as InputIter>::Item>,
{
    preceded(tag(control), recognize(one_of(specials)))
}

/// Parse and result in Expr
///
/// This implementation does not adjust according to the quote style
/// The following character carry special meaning in parameter expansions
fn parse_expr<'a>(input: &'a str) -> IResult<&'a str, Expr<'a>> {
    const SPECIALS: &str = "\\\"$}";

    fold_many0(
        alt((
            map(is_not(SPECIALS), |s| Expr::Str(s)), //
            map(escaped_str("\\", SPECIALS), |s| Expr::Str(s)),
            map(parse_expansion, |s| Expr::Substiution(s.into())),
        )),
        || Expr::Str(&""),
        combine_expr,
    )(input)
}

fn parse_expansion_kind(input: &str) -> IResult<&str, ExpansionKind> {
    macro_rules! expr_kind {
        ($tag:literal, $kind:path) => {
            map(preceded(tag($tag), parse_expr), |expr| $kind(expr))
        };
    }

    alt((
        map(peek(tag("}")), |_| ExpansionKind::Simple),
        expr_kind!(":-", ExpansionKind::Default),
        expr_kind!("-", ExpansionKind::DefaultNull),
        expr_kind!(":+", ExpansionKind::Alt),
        expr_kind!("+", ExpansionKind::AltNull),
        // expr_kind!("?+", ExpansionKind::Err),
        // expr_kind!("?", ExpansionKind::ErrNull),
    ))(input)
}

/// Parse parameter expansion
fn parse_expansion(input: &str) -> IResult<&str, Expansion> {
    map(
        delimited(
            tag("${"), // start
            tuple((
                parse_identifier, // id
                parse_expansion_kind,
            )),
            tag("}"),
        ),
        |(name, kind)| Expansion { name, kind },
    )(input)
}

/// Parse RHS in single quotes
fn parse_rhs_single(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(
            tag("'"), // starting quote
            escaped_transform(
                none_of("'\\"),
                '\'',
                alt((
                    value("\\", tag("\\")), // Backslash
                    value("'", tag("'")),   // single quote
                )),
            ),
            tag("'"), // ending quote
        ),
        |s| Expr::String(s),
    )(input)
}

/// Parse unquoted RHS
fn parse_rhs_unqouted(input: &str) -> IResult<&str, Expr> {
    // why concat!() do not support const 'static str?
    const ESCAPABLES: &str = " \t$\\\"'";
    const TERMINATORS: &str = " \t$\\\"'\r\n";

    fold_many0(
        alt((
            map(is_not(TERMINATORS), |s| Expr::Str(s)), //
            map(escaped_str("\\", ESCAPABLES), |s| Expr::Str(s)),
            map(parse_expansion, |s| Expr::Substiution(s.into())),
        )),
        || Expr::Str(""),
        combine_expr,
    )(input)
}

/// Parse RHS in double quotes
fn parse_rhs_double(input: &str) -> IResult<&str, Expr> {
    const ESCAPABLES: &str = "$\"";
    const TERMINATORS: &str = ESCAPABLES;

    delimited(
        tag("\""), // start qoute
        fold_many0(
            alt((
                map(is_not(TERMINATORS), |s| Expr::Str(s)), //
                map(escaped_str("\\", ESCAPABLES), |s| Expr::Str(s)),
                map(parse_expansion, |s| Expr::Substiution(s.into())),
            )), //
            || Expr::Str(""),
            combine_expr,
        ),
        tag("\""),
    )(input)
}

fn parse_rhs<'a>(input: &'a str) -> IResult<&'a str, Expr<'a>> {
    alt((
        parse_rhs_single, //
        parse_rhs_double,
        parse_rhs_unqouted,
    ))(input)
}

/// Parse an item from input
///
/// The result may be Some((name, expr)), or None if comment or whitespaces
pub(crate) fn parse(input: &str) -> IResult<&str, Option<(&str, Expr)>> {
    alt((
        map(multispace1, |_| None), // whitespaces
        map(
            // comment
            preceded(tag("#"), take_till(|c| c == '\n')),
            |_| None,
        ),
        map(
            // assignment
            separated_pair(parse_identifier, tag("="), parse_rhs),
            |s| Some(s),
        ),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Expansion;
    use crate::parser::ExpansionKind;
    use crate::parser::Expr;
    use crate::parser::parse_expansion;

    #[test]
    fn expansion_simple() {
        let input = "${VAR}";
        let result = parse_expansion(input);
        assert_eq!(
            result,
            Ok((
                "",
                Expansion {
                    name: "VAR",
                    kind: ExpansionKind::Simple,
                }
            ))
        );
    }

    #[test]
    fn expansion_default() {
        let input = "${VAR:-default}";
        let result = parse_expansion(input);
        assert_eq!(
            result,
            Ok((
                "",
                Expansion::new("VAR", ExpansionKind::Default(Expr::Str("default"))),
            ))
        );
    }

    #[test]
    fn expansion_nested() {
        let input = "${VAR:-${INNER}}";
        let result = parse_expansion(input);
        assert_eq!(
            result,
            Ok((
                "",
                Expansion::new(
                    "VAR",
                    ExpansionKind::Default(Expr::Substiution(
                        Expansion::new("INNER", ExpansionKind::Simple).into()
                    ))
                ),
            ))
        );
    }

    #[test]
    fn expansion_escaped() {
        let input = "${VAR:-\\}\\$}";
        let result = parse_expansion(input);
        assert_eq!(
            result,
            Ok((
                "",
                Expansion::new("VAR", ExpansionKind::Default(Expr::String("}$".to_owned()))),
            ))
        );
    }

    #[test]
    fn expr_composite() {
        let input = "a${VAR}b";
        let result = parse_rhs(input);
        assert_eq!(
            result,
            Ok((
                "",
                Expr::Composite(vec![
                    Expr::Str("a"),
                    Expr::Substiution(Expansion::new("VAR", ExpansionKind::Simple).into()),
                    Expr::Str("b"),
                ])
            ))
        );
    }

    #[test]
    fn expr_escaped() {
        let input = "a\\$${VAR}b";
        let result = parse_rhs(input);
        assert_eq!(
            result,
            Ok((
                "",
                Expr::Composite(vec![
                    Expr::String("a$".to_owned()),
                    Expr::Substiution(Expansion::new("VAR", ExpansionKind::Simple).into()),
                    Expr::Str("b"),
                ])
            ))
        );
    }
}
