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

use crate::syntax::AstFragment;

pub type Ident = SmolStr;

pub type Term = AstTerm<Ident>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AstTerm<Id> {
    Abstraction(Id, Box<AstTerm<Id>>),
    Application(Box<AstTerm<Id>>, Box<AstTerm<Id>>),
    Variable(Id),
    Let(Id, Box<AstTerm<Id>>, Box<AstTerm<Id>>),
    MacroDef(Id, Box<AstFragment<Id>>),
}

pub type Path = SmolStr;

pub type Stmt = AstStmt<Ident>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum AstStmt<Id> {
    Expr(AstTerm<Id>),
    Bind(Id, AstTerm<Id>),
    Import(Path),
}

impl<Id> From<AstTerm<Id>> for AstStmt<Id> {
    fn from(term: AstTerm<Id>) -> Self {
        Self::Expr(term)
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
            Term::MacroDef(arg, _body) => write!(f, "(macro {arg}. )"),
        }
    }
}
