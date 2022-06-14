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
    Abstraction(Abstraction),
    Application(Application),
    Variable(Variable),
}

impl From<Application> for Term {
    fn from(application: Application) -> Self {
        Term::Application(application)
    }
}

impl From<Abstraction> for Term {
    fn from(abstraction: Abstraction) -> Self {
        Term::Abstraction(abstraction)
    }
}

impl From<Variable> for Term {
    fn from(variable: Variable) -> Self {
        Term::Variable(variable)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Abstraction {
    pub arg: Variable,
    pub body: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Application {
    pub apply: Box<Term>,
    pub arg: Box<Term>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Variable {
    pub ident: Ident,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Ident(pub String);
