use std::{collections::BTreeSet, fmt::Display, sync::atomic::AtomicU64};

use crate::ast::{self, AstStmt, AstTerm, Ident, Stmt, Term};

pub type SyntaxTerm = AstTerm<Syntax>;
pub type SyntaxStmt = AstStmt<Syntax>;

impl Display for SyntaxTerm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstTerm::Abstraction(arg, body) => write!(f, "(λ {arg}. {body})"),
            AstTerm::Application(lhs, rhs) => write!(f, "({lhs} {rhs})"),
            AstTerm::Variable(var) => write!(f, "{var}"),
            AstTerm::Let(bind, val, body) => write!(f, "let {bind} = {val} in {body}"),
            AstTerm::MacroDef(arg, body) => write!(f, "{arg}, {body}"),
        }
    }
}

impl Display for SyntaxFragment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            AstFragment::Term(term) => write!(f, "{term}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub struct Syntax {
    ident: Ident,
    scopes: BTreeSet<Scope>,
}

impl Display for Syntax {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.ident)
    }
}

impl From<Ident> for Syntax {
    fn from(ident: Ident) -> Self {
        Self {
            ident,
            scopes: BTreeSet::new(),
        }
    }
}

impl Syntax {
    pub fn new(ident: Ident, scopes: BTreeSet<Scope>) -> Self {
        Self { ident, scopes }
    }
    pub fn ident(&self) -> Ident {
        self.ident.clone()
    }
    pub fn subset_of(&self, other: &Syntax) -> bool {
        self.scopes.is_subset(&other.scopes)
    }
    pub fn add_scope(&mut self, scope: Scope) {
        self.scopes.insert(scope);
    }
    pub fn remove_scope(&mut self, scope: &Scope) {
        self.scopes.remove(scope);
    }
    pub fn flip_scope(&mut self, scope: Scope) {
        if self.scopes.contains(&scope) {
            self.scopes.remove(&scope);
        } else {
            self.scopes.insert(scope);
        }
    }
    pub fn num_scopes(&self) -> usize {
        self.scopes.len()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash, PartialOrd, Ord)]
pub struct Scope {
    id: u64,
}

pub type SyntaxFragment = AstFragment<Syntax>;

pub type Fragment = AstFragment<Ident>;

pub trait HasSyntax {
    fn alter_syntax<F>(&mut self, op: F)
    where
        F: Fn(&mut Syntax) + Copy;

    fn add_scope(&mut self, scope: Scope) {
        let scope_cloned = scope.clone();
        let foo = |s: &mut Syntax| s.add_scope(scope_cloned.clone());
        self.alter_syntax(foo);
    }

    fn remove_scope(&mut self, scope: &Scope) {
        self.alter_syntax(|syn| syn.remove_scope(scope));
    }

    fn flip_scope(&mut self, scope: Scope) {
        self.alter_syntax(|syn| syn.flip_scope(scope.clone()));
    }
}

impl HasSyntax for Syntax {
    fn alter_syntax<F>(&mut self, op: F)
    where
        F: Fn(&mut Syntax) + Copy,
    {
        op(self)
    }
}

impl HasSyntax for SyntaxTerm {
    fn alter_syntax<F>(&mut self, op: F)
    where
        F: Fn(&mut Syntax) + Copy,
    {
        match self {
            Self::Abstraction(arg, body) => {
                arg.alter_syntax(op);
                body.alter_syntax(op);
            }
            Self::Application(lhs, rhs) => {
                lhs.alter_syntax(op);
                rhs.alter_syntax(op);
            }
            Self::Variable(syntax) => syntax.alter_syntax(op),
            Self::Let(syn, val, body) => {
                syn.alter_syntax(op);
                val.alter_syntax(op);
                body.alter_syntax(op);
            }
            Self::MacroDef(syn, frag) => {
                syn.alter_syntax(op);
                frag.alter_syntax(op);
            }
        }
    }
}

impl HasSyntax for SyntaxFragment {
    fn alter_syntax<F>(&mut self, op: F)
    where
        F: Fn(&mut Syntax) + Copy,
    {
        match self {
            Self::Term(term) => term.alter_syntax(op),
        }
    }
}

impl HasSyntax for SyntaxStmt {
    fn alter_syntax<F>(&mut self, op: F)
    where
        F: Fn(&mut Syntax) + Copy,
    {
        match self {
            AstStmt::Expr(e) => e.alter_syntax(op),
            AstStmt::Bind(id, body) => {
                id.alter_syntax(op);
                body.alter_syntax(op);
            }
            AstStmt::Import(_) => {}
        }
    }
}

impl<T> HasSyntax for Box<T>
where
    T: HasSyntax,
{
    fn alter_syntax<F>(&mut self, op: F)
    where
        F: Fn(&mut Syntax) + Copy,
    {
        (**self).alter_syntax(op)
    }
}

impl<T> HasSyntax for Vec<T>
where
    T: HasSyntax,
{
    fn alter_syntax<F>(&mut self, op: F)
    where
        F: Fn(&mut Syntax) + Copy,
    {
        self.iter_mut().for_each(|it| it.alter_syntax(op))
    }
}

pub trait IntoSyntax {
    type Output: HasSyntax;

    fn into_syntax(self) -> Self::Output;
}

pub trait IntoDatum {
    type Output;

    fn into_datum(self) -> Self::Output;
}

impl<T> IntoDatum for Box<T>
where
    T: IntoDatum,
{
    type Output = Box<<T as IntoDatum>::Output>;

    fn into_datum(self) -> Self::Output {
        Box::new((*self).into_datum())
    }
}

impl<T> IntoDatum for Vec<T>
where
    T: IntoDatum,
{
    type Output = Vec<<T as IntoDatum>::Output>;

    fn into_datum(self) -> Self::Output {
        self.into_iter().map(IntoDatum::into_datum).collect()
    }
}

impl IntoDatum for Syntax {
    type Output = Ident;

    fn into_datum(self) -> Self::Output {
        self.ident()
    }
}

impl IntoDatum for SyntaxTerm {
    type Output = Term;
    fn into_datum(self) -> Term {
        match self {
            AstTerm::Abstraction(id, body) => Term::Abstraction(id.into_datum(), body.into_datum()),
            AstTerm::Application(lhs, rhs) => Term::Application(lhs.into_datum(), rhs.into_datum()),
            AstTerm::Variable(id) => Term::Variable(id.into_datum()),
            AstTerm::Let(id, val, body) => {
                Term::Let(id.into_datum(), val.into_datum(), body.into_datum())
            }
            AstTerm::MacroDef(arg, body) => Term::MacroDef(arg.into_datum(), body.into_datum()),
        }
    }
}

impl IntoDatum for SyntaxStmt {
    type Output = Stmt;

    fn into_datum(self) -> Self::Output {
        match self {
            AstStmt::Expr(e) => Stmt::Expr(e.into_datum()),
            AstStmt::Bind(id, body) => Stmt::Bind(id.into_datum(), body.into_datum()),
            AstStmt::Import(path) => Stmt::Import(path),
        }
    }
}

impl IntoDatum for SyntaxFragment {
    type Output = Fragment;
    fn into_datum(self) -> Fragment {
        match self {
            SyntaxFragment::Term(term) => Fragment::Term(term.into_datum()),
        }
    }
}

impl<T> IntoSyntax for Box<T>
where
    T: IntoSyntax,
{
    type Output = Box<<T as IntoSyntax>::Output>;
    fn into_syntax(self) -> Self::Output {
        Box::new((*self).into_syntax())
    }
}

impl<T> IntoSyntax for Vec<T>
where
    T: IntoSyntax,
{
    type Output = Vec<<T as IntoSyntax>::Output>;

    fn into_syntax(self) -> Self::Output {
        self.into_iter().map(IntoSyntax::into_syntax).collect()
    }
}

impl IntoSyntax for Ident {
    type Output = Syntax;

    fn into_syntax(self) -> Self::Output {
        self.into()
    }
}

impl IntoSyntax for Term {
    type Output = SyntaxTerm;
    fn into_syntax(self) -> SyntaxTerm {
        match self {
            AstTerm::Abstraction(id, body) => {
                SyntaxTerm::Abstraction(id.into_syntax(), body.into_syntax())
            }
            AstTerm::Application(lhs, rhs) => {
                SyntaxTerm::Application(lhs.into_syntax(), rhs.into_syntax())
            }
            AstTerm::Variable(id) => SyntaxTerm::Variable(id.into_syntax()),
            AstTerm::Let(id, val, body) => {
                SyntaxTerm::Let(id.into_syntax(), val.into_syntax(), body.into_syntax())
            }
            AstTerm::MacroDef(arg, body) => {
                SyntaxTerm::MacroDef(arg.into_syntax(), body.into_syntax())
            }
        }
    }
}

impl IntoSyntax for Stmt {
    type Output = SyntaxStmt;

    fn into_syntax(self) -> Self::Output {
        match self {
            AstStmt::Expr(e) => SyntaxStmt::Expr(e.into_syntax()),
            AstStmt::Bind(id, body) => SyntaxStmt::Bind(id.into_syntax(), body.into_syntax()),
            AstStmt::Import(path) => AstStmt::Import(path),
        }
    }
}

impl IntoSyntax for Fragment {
    type Output = SyntaxFragment;
    fn into_syntax(self) -> SyntaxFragment {
        match self {
            Fragment::Term(term) => SyntaxFragment::Term(term.into_syntax()),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, PartialOrd, Ord)]
pub enum AstFragment<Id> {
    // Stmt(ast::Stmt),
    Term(ast::AstTerm<Id>),
}

impl Scope {
    pub fn new() -> Self {
        static ID: AtomicU64 = AtomicU64::new(0);
        Self {
            id: ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        }
    }

    pub fn with_id(id: u64) -> Self {
        Self { id }
    }
}
