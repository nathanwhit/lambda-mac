/*
Grammar:

Term    = Abs | App | Var | "(" Term ")"
λ       = ("lambda" ws) | "λ" | "\"
Abs     =  λ Var "." Term
App     = Term ws Term
Ident   = ("_" alpha) alphanum+
Var     = Ident
*/

use std::fmt::{self, Display};

use smol_str::SmolStr;

pub type Ident = SmolStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Abstraction(Ident, Box<Term>),
    Application(Box<Term>, Box<Term>),
    Variable(Ident),
    Let(Ident, Box<Term>, Box<Term>),
}

pub type Path = SmolStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Expr(Term),
    Bind(Ident, Term),
    Import(Path),
}

impl From<Term> for Stmt {
    fn from(term: Term) -> Self {
        Stmt::Expr(term)
    }
}

impl Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Term::Abstraction(arg, body) => {
                write!(f, "(λ{arg}. {body})")
            }
            Term::Application(lhs, rhs) => write!(f, "({lhs} {rhs})"),
            Term::Variable(name) => write!(f, "{name}"),
            Term::Let(name, value, body) => write!(f, "let {name} = {value} in {body}"),
        }
    }
}
