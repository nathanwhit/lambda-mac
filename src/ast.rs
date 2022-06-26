/*
Grammar:

Term    = Abs | App | Var | "(" Term ")"
λ       = ("lambda" ws) | "λ" | "\"
Abs     =  λ Var "." Term
App     = Term ws Term
Ident   = ("_" alpha) alphanum+
Var     = Ident
*/

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
