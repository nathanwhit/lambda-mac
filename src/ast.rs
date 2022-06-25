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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Abstraction(SmolStr, Box<Term>),
    Application(Box<Term>, Box<Term>),
    Variable(SmolStr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Expr(Term),
    Bind(SmolStr, Term),
}

impl From<Term> for Stmt {
    fn from(term: Term) -> Self {
        Stmt::Expr(term)
    }
}
