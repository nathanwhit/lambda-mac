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
