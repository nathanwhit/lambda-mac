/*
Grammar:

Term    = Abs | App | Var | "(" Term ")"
λ       = ("lambda" ws) | "λ" | "\"
Abs     =  λ Var "." Term
App     = Term ws Term
Ident   = ("_" alpha) alphanum+
Var     = Ident
*/

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    Abstraction(String, Box<Term>),
    Application(Box<Term>, Box<Term>),
    Variable(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stmt {
    Expr(Term),
    Bind(String, Term),
}

impl From<Term> for Stmt {
    fn from(term: Term) -> Self {
        Stmt::Expr(term)
    }
}
