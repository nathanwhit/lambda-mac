use crate::ast::Stmt;
use crate::ast::Term;

use nom::branch::alt;
use nom::character::complete::alphanumeric1;
use nom::character::complete::multispace0;
use nom::character::complete::newline;
use nom::character::complete::space0;
use nom::character::complete::{alpha1, multispace1};
use nom::combinator::eof;
use nom::combinator::recognize;
use nom::combinator::verify;
use nom::error::ParseError;
use nom::error::VerboseError;
use nom::multi::many1;
use nom::multi::{fold_many1, many0_count};
use nom::sequence::delimited;
use nom::sequence::pair;
use nom::IResult;
use nom::Parser;
use nom_supreme::tag::complete::tag;
use nom_supreme::tag::TagError;
use nom_supreme::ParserExt;
use smol_str::SmolStr;
// use nom_supreme::ParserExt;

type ParseResult<'a, O, E = VerboseError<&'a str>> = IResult<&'a str, O, E>;
type Input<'a> = &'a str;

pub fn program(input: Input<'_>) -> ParseResult<'_, Vec<Stmt>> {
    many1(statement.context("statement"))
        .context("program statements")
        .parse(input)
}

fn chomp_newlines(input: Input<'_>) -> ParseResult<'_, usize> {
    many0_count(newline)(input)
}

pub fn statement(input: Input<'_>) -> ParseResult<'_, Stmt> {
    let (input, stmt) = stmt.context("stmt").parse(input)?;
    let (input, _) =
        ws(alt((tag(";").context("semicolon"), eof.context("eof")))
            .context("either semicolon or end"))(input)?;
    let (input, _) = chomp_newlines(input)?;
    Ok((input, stmt))
}

pub fn stmt(input: Input<'_>) -> ParseResult<'_, Stmt> {
    ws(alt((
        bind.context("binding"),
        term.context("term").map(Stmt::Expr),
    )))(input)
}

pub fn expr(input: Input<'_>) -> ParseResult<'_, Stmt> {
    term.map(Stmt::Expr).parse(input)
}

pub fn bind(input: Input<'_>) -> ParseResult<'_, Stmt> {
    let (input, name) = ws(ident)(input)?;
    let (input, _) = ws(tag("="))(input)?;
    let (input, term) = ws(term)(input)?;
    let (input, _) = space0(input)?;

    Ok((input, Stmt::Bind(name, term)))
}

#[tracing::instrument]
pub fn term(input: Input<'_>) -> ParseResult<'_, Term> {
    ws(alt((
        let_term.context("let"),
        abstraction,
        application,
        variable,
        parenthesized(term),
    )))(input)
}

#[tracing::instrument]
pub fn let_term(input: Input<'_>) -> ParseResult<'_, Term> {
    let (input, _) = ws(tag("let"))(input)?;
    let (input, name) = ws(ident)(input)?;
    let (input, _) = ws(tag("="))(input)?;
    let (input, value) = ws(term)(input)?;
    let (input, _) = ws(tag("in"))(input)?;
    let (input, body) = ws(term)(input)?;
    Ok((input, Term::Let(name, Box::new(value), Box::new(body))))
}

/// A combinator that takes a parser `inner` and produces a parser that also consumes both leading and
/// trailing whitespace, returning the output of `inner`.
#[tracing::instrument(skip(inner))]
fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> ParseResult<O, E>
where
    F: Parser<&'a str, O, E>,
{
    delimited(space0, inner, space0)
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

const KEYWORDS: [&'static str; 2] = ["let", "in"];

#[tracing::instrument]
pub fn ident(input: Input<'_>) -> ParseResult<'_, SmolStr> {
    verify(identifier, |id| !KEYWORDS.contains(id))
        .map(|id| id.into())
        .parse(input)
}

#[cfg(test)]
mod tests {
    use tracing::dispatcher::DefaultGuard;
    use tracing_subscriber::{prelude::*, EnvFilter, Registry};
    use tracing_tree::HierarchicalLayer;

    use super::*;

    macro_rules! assert_parse {
        ($thing:expr, $expect:expr) => {
            ::pretty_assertions::assert_eq!($thing.map_err(|_| ()), $expect)
        };
    }

    macro_rules! parse_tests {
        ($($name: ident : $input: expr => $output: expr);* $(;)?) => {
            paste::paste! {
                $(
                    #[test]
                    fn [<test_parse_ $name>]() {
                        assert_parse!($input, $output);
                    }
                )*
            }
        };
    }

    macro_rules! id {
        ($ident: ident) => {
            ::smol_str::SmolStr::from(stringify!($ident))
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

    macro_rules! bind {
        ($name: ident = $val: expr) => {
            $crate::ast::Stmt::Bind(id!($name), $val.into())
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
    fn thing() {
        assert_parses! {
            term("((λx. λy. x) y) z") => Ok(("", app!(app!(abs!(x -> abs!(y -> var!(x))), var!(y)), var!(z))))
        }
    }

    parse_tests! {
        // identifier
        basic_identifier                :   identifier("abc")            => Ok(("", "abc"));
        identifier_leading_underscore   :   identifier("_")              => Ok(("", "_"));
        identifier_numerics             :   identifier("_abc_123")       => Ok(("", "_abc_123"));
        // ident
        basic_ident                     :   ident("abc")                 => Ok(("", id!(abc)));
        ident_leading_numeric           :   ident("3abc")                => Err(());
        // variable
        variable_basic                  :   variable("abc")              => Ok(("", var!(abc)));

        // abstraction
        basic_lambda_text               :   abstraction("lambda x . x")  => Ok(("", abs!(x -> var!(x))));
        basic_lambda_no_ws              :   abstraction("λx.x")          => Ok(("", abs!(x -> var!(x))));
        basic_lambda_ws                 :   abstraction("λx. x")         => Ok(("", abs!(x -> var!(x))));
        basic_lambda_ws2                :   abstraction("λ x. x")        => Ok(("", abs!(x -> var!(x))));
        lamda_text_ws                   :   abstraction("lambda x. x")   => Ok(("", abs!(x -> var!(x))));
        lambda_text_no_ws               :   abstraction("lambdax. x")    => Err(());
        nested_lambda                   :   abstraction("λx. λy. y")     => Ok(("", abs!(x -> abs!(y -> var!(y)))));
        backslash_lambda                :   abstraction(r"\x. x")        => Ok(("", abs!(x -> var!(x))));

        // term
        term_abs                        :   term("λx. x")                => Ok(("", abs!(x -> var!(x)).into()));
        id_app                          :   term("(λx. x) y")            => Ok(("", app!(abs!(x -> var!(x)), var!(y)).into()));
        id_app_two                      :   term("(λx. x) y z")          => Ok(("", app!(app!(abs!(x -> var!(x)), var!(y)), var!(z)).into()));
        app_multiple                    :   term("((λx. x) y) z")        => Ok(("", app!(app!(abs!(x -> var!(x)), var!(y)), var!(z)).into()));

        // stmt
        stmt_abs                        :   stmt("λx. x")                => Ok(("", abs!(x -> var!(x)).into()));
        stmt_bind                       :   stmt("id = λx. x")           => Ok(("", bind!(id = abs!(x -> var!(x)))));

        // associativity
        application_is_left_assoc       :   application("s t u")         => Ok(("", app!(app!(var!(s), var!(t)), var!(u)).into()));
        abstraction_is_right_assoc      :   abstraction("λx. λy. x y")   => Ok(("", abs!(x -> abs!(y -> app!(var!(x), var!(y))))));

        // misc
        thing                           :   term("((λx. λy. x) y) z")    => Ok(("", app!(app!(abs!(x -> abs!(y -> var!(x))), var!(y)), var!(z))));

        // program
        basic_program                   :   program("foo = x; bar = y;") => Ok(("", vec![bind!(foo = var!(x)), bind!(bar = var!(y))]));
    }
}
