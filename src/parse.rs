use crate::ast::Term;

use nom::branch::alt;
use nom::character::complete::alphanumeric1;
use nom::character::complete::multispace0;
use nom::character::complete::{alpha1, multispace1};
use nom::combinator::recognize;
use nom::error::ParseError;
use nom::multi::{fold_many1, many0_count};
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::IResult;
use nom::Parser;
use nom_supreme::error::ErrorTree;
use nom_supreme::tag::complete::tag;
use nom_supreme::tag::TagError;
// use nom_supreme::ParserExt;

type ParseResult<'a, O, E = ErrorTree<&'a str>> = IResult<&'a str, O, E>;
type Input<'a> = &'a str;

#[tracing::instrument]
pub fn term(input: Input<'_>) -> ParseResult<'_, Term> {
    ws(alt((
        abstraction,
        application,
        variable,
        parenthesized(term),
    )))(input)
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
#[tracing::instrument(skip(inner))]
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> ParseResult<O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

#[tracing::instrument]
fn identifier(input: Input<'_>) -> ParseResult<'_, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0_count(alt((alphanumeric1, tag("_")))),
    ))(input)
}

#[tracing::instrument(skip(inner))]
fn parenthesized<'a, P, O, E>(inner: P) -> impl FnMut(&'a str) -> ParseResult<'a, O, E>
where
    P: Parser<&'a str, O, E>,
    E: ParseError<&'a str> + TagError<&'a str, &'static str>,
{
    delimited(tag("("), inner, tag(")"))
}

#[tracing::instrument]
pub fn atomic_term(input: Input<'_>) -> ParseResult<'_, Term> {
    ws(alt((
        parenthesized(abstraction).map(From::from),
        parenthesized(application).map(From::from),
        variable.map(From::from),
    )))(input)
}

#[tracing::instrument]
pub fn abstraction(input: Input<'_>) -> ParseResult<'_, Term> {
    let arg = ident;
    let body = term;

    let lam = |input| {
        let (rest, _) = alt((tag("λ"), tag("\\")))(input)?;
        multispace0(rest)
    };
    let lambda = |input| {
        let (rest, _) = tag("lambda")(input)?;
        multispace1(rest)
    };

    let (rest, _) = alt((lam, lambda))(input)?;
    let (rest, _) = multispace0(rest)?;
    let (rest, arg) = arg(rest)?;
    let (rest, _) = ws(tag("."))(rest)?;
    let (rest, body) = body(rest)?;

    Ok((rest, Term::Abstraction(arg, Box::new(body))))
}

#[tracing::instrument]
pub fn application(input: Input<'_>) -> ParseResult<'_, Term> {
    let (rest, first) = atomic_term(input)?;
    fold_many1(
        atomic_term,
        move || first.clone(),
        |acc, curr| Term::Application(Box::new(acc), Box::new(curr)),
    )(rest)
}

#[tracing::instrument]
pub fn variable(input: Input<'_>) -> ParseResult<'_, Term> {
    ident.map(|ident| Term::Variable(ident)).parse(input)
}

#[tracing::instrument]
pub fn ident(input: Input<'_>) -> ParseResult<'_, String> {
    identifier.map(|id| id.into()).parse(input)
}

#[cfg(test)]
mod tests {
    use tracing::dispatcher::DefaultGuard;
    use tracing_subscriber::{prelude::__tracing_subscriber_SubscriberExt, EnvFilter, Registry};
    use tracing_tree::HierarchicalLayer;

    use super::*;

    macro_rules! assert_parse {
        ($thing:expr, $expect:expr) => {
            ::pretty_assertions::assert_eq!($thing.map_err(|_| ()), $expect)
        };
    }

    macro_rules! id {
        ($ident: ident) => {
            String::from(stringify!($ident))
        };
    }

    macro_rules! var {
        ($ident: ident) => {
            $crate::ast::Term::Variable(id!($ident))
        };
    }

    macro_rules! abs {
        ($v: ident -> $b: expr) => {
            $crate::ast::Term::Abstraction(id!($v), Box::new($b.into()))
        };
    }

    macro_rules! app {
        ($a: expr, $b: expr) => {
            $crate::ast::Term::Application(Box::new($a.into()), Box::new($b.into()))
        };
    }

    macro_rules! assert_parses {
        ($($thing: expr => $expect: expr),* $(,)?) => {
            $(
                assert_parse!($thing, $expect);
            )*
        };
    }

    #[allow(dead_code)]
    fn init_tracing() -> DefaultGuard {
        let subscriber = Registry::default()
            .with(EnvFilter::from_env("RUST_LOG"))
            .with(HierarchicalLayer::new(2).with_indent_lines(true));
        tracing::subscriber::set_default(subscriber)
    }

    #[test]
    fn identifier_works() {
        assert_parses! {
            identifier("abc")       => Ok(("", "abc")),
            identifier("_")         => Ok(("", "_")),
            identifier("_abc")      => Ok(("", "_abc")),
            identifier("_abc_")     => Ok(("", "_abc_")),
            identifier("_abc_123")  => Ok(("", "_abc_123")),
        }
    }

    #[test]
    fn ident_works() {
        assert_parses! {
            ident("abc")            => Ok(("", id!(abc))),
            ident("3abc")           => Err(()),
        }
    }

    #[test]
    fn variable_works() {
        assert_parses! {
            variable("abc")         => Ok(("", var!(abc))),
        }
    }

    #[test]
    fn abstraction_works() {
        assert_parses! {
            abstraction("lambda x . x")  => Ok(("", abs!(x -> var!(x)))),
            abstraction("λx.x")          => Ok(("", abs!(x -> var!(x)))),
            abstraction("λx. x")         => Ok(("", abs!(x -> var!(x)))),
            abstraction("λ x. x")        => Ok(("", abs!(x -> var!(x)))),
            abstraction("lambda x. x")   => Ok(("", abs!(x -> var!(x)))),
            abstraction("lambdax. x")    => Err(()),
            abstraction("λx. λy. y")     => Ok(("", abs!(x -> abs!(y -> var!(y))))),
            abstraction(r"\x. x")        => Ok(("", abs!(x -> var!(x)))),
        }
    }

    #[test]
    fn application_works() {
        assert_parses! {
            application("(λx. x) y")     => Ok(("", app!(abs!(x -> var!(x)), var!(y)).into())),
            application("λx. x y")       => Err(()),
        }
    }

    #[test]
    fn term_works() {
        assert_parses! {
            term("λx. x")                => Ok(("", abs!(x -> var!(x)).into())),
            term("(λx. x) y")            => Ok(("", app!(abs!(x -> var!(x)), var!(y)).into())),
            term("(λx. x) y z")          => Ok(("", app!(app!(abs!(x -> var!(x)), var!(y)), var!(z)).into())),
            term("((λx. x) y) z")        => Ok(("", app!(app!(abs!(x -> var!(x)), var!(y)), var!(z)).into())),
        }
    }

    #[test]
    fn application_is_left_associative() {
        assert_parses! {
            application("s t u") => Ok(("", app!(app!(var!(s), var!(t)), var!(u)).into())),
            // application
        }
    }

    #[test]
    fn abstraction_is_right_associative() {
        assert_parses! {
            abstraction("λx. λy. x y") => Ok(("", abs!(x -> abs!(y -> app!(var!(x), var!(y))))))
        }
    }

    #[test]
    fn thing() {
        assert_parses! {
            term("((λx. λy. x) y) z") => Ok(("", app!(app!(abs!(x -> abs!(y -> var!(x))), var!(y)), var!(z))))
        }
    }
}
