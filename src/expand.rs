use std::{fmt::Debug, ops::Not, sync::atomic::AtomicU64};

use crate::{
    ast::Stmt,
    syntax::{
        HasSyntax, IntoDatum, IntoSyntax, Scope, Syntax, SyntaxFragment, SyntaxStmt, SyntaxTerm,
    },
};
use color_eyre::Result;
use im::HashMap;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct Binding {
    id: u64,
}

impl Binding {
    pub fn new() -> Self {
        static ID: AtomicU64 = AtomicU64::new(0);
        Self {
            id: ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        }
    }
}

pub struct Expander {
    program: Vec<SyntaxStmt>,
    context: ExpansionContext,
}

impl Expander {
    pub fn new(program: Vec<Stmt>) -> Self {
        Self {
            program: program.into_syntax(),
            context: ExpansionContext::new(),
        }
    }

    pub fn expand(self) -> Result<Vec<Stmt>> {
        let Self {
            program,
            mut context,
        } = self;
        let mut output = Vec::new();
        for stmt in program {
            tracing::debug!(?stmt, "expanding statment");
            let result = context.expand_stmt(stmt)?;
            if let Some(stmt) = result {
                output.push(stmt);
            }
        }
        Ok(output.into_datum())
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
struct MacroDef {
    arg: Syntax,
    body: Box<SyntaxFragment>,
}

// #[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
// enum ItemKind {
//     Variable,
//     Abstraction,
//     Other,
//     MacroDef(MacroDef),
// }

#[derive(Debug)]
pub struct ExpansionContext {
    macros: HashMap<Binding, MacroDef>,
    // env: HashMap<Binding, ItemKind>,
    bindings: HashMap<Syntax, Binding>,
    global_scopes: Vec<Scope>,
}

macro_rules! try_opt {
    ($exp:expr) => {
        match $exp {
            Some(e) => e,
            None => return Ok(None),
        }
    };
}

impl ExpansionContext {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            // env: HashMap::new(),
            bindings: HashMap::new(),
            global_scopes: Vec::new(),
        }
    }
    fn add_global_scope(&mut self, scope: Scope) {
        self.global_scopes.push(scope);
    }

    // #[tracing::instrument]
    fn apply_global_scopes<T>(&self, to: &mut T)
    where
        T: HasSyntax + Debug,
    {
        for scope in &self.global_scopes {
            to.add_scope(scope.clone());
        }
    }
    fn add_binding(&mut self, id: Syntax, binding: Binding) {
        self.bindings.insert(id, binding);
    }
    fn resolve(&mut self, id: &Syntax) -> Result<Option<Binding>> {
        let candidates: Vec<_> = self
            .bindings
            .keys()
            .filter(|&can| can.ident() == id.ident() && can.subset_of(&id))
            .collect();
        let best = if let Some(&best) = candidates.iter().max_by_key(|&&k| k.num_scopes()) {
            best
        } else {
            return Ok(None);
        };

        if candidates
            .into_iter()
            .any(|cand| cand.subset_of(best).not())
        {
            return Err(color_eyre::eyre::eyre!("ambiguous: {id:?}"));
        }

        Ok(Some(
            self.bindings
                .get(best)
                .expect("we just got it from the binding table!")
                .clone(),
        ))
    }

    #[tracing::instrument]
    pub fn expand_stmt(&mut self, mut stmt: SyntaxStmt) -> Result<Option<SyntaxStmt>> {
        self.apply_global_scopes(&mut stmt);
        Ok(Some(match stmt {
            crate::ast::AstStmt::Expr(e) => SyntaxStmt::Expr(try_opt!(self.expand_term(e)?)),
            crate::ast::AstStmt::Bind(mut id, SyntaxTerm::MacroDef(mut arg, mut body)) => {
                let scope = Scope::new();
                id.add_scope(scope.clone());
                self.add_binding(id.clone(), Binding::new());
                self.add_global_scope(scope);

                let scope = Scope::new();
                arg.add_scope(scope.clone());
                body.add_scope(scope);
                let binding = Binding::new();
                self.add_binding(id, binding.clone());
                self.macros.insert(binding, MacroDef { arg, body });
                return Ok(None);
            }
            crate::ast::AstStmt::Bind(mut id, value) => {
                let scope = Scope::new();
                id.add_scope(scope.clone());
                self.add_binding(id.clone(), Binding::new());
                self.add_global_scope(scope);

                let scope = Scope::new();
                id.add_scope(scope);
                self.add_binding(id.clone(), Binding::new());
                SyntaxStmt::Bind(id, try_opt!(self.expand_term(value)?))
            }
            crate::ast::AstStmt::Import(path) => SyntaxStmt::Import(path),
        }))
    }

    fn replace_term(
        &mut self,
        mut term: SyntaxTerm,
        var: Syntax,
        replacement: SyntaxTerm,
    ) -> Result<SyntaxTerm> {
        self.apply_global_scopes(&mut term);
        Ok(match term {
            crate::ast::AstTerm::Abstraction(arg, body) => {
                SyntaxTerm::Abstraction(arg, Box::new(self.replace_term(*body, var, replacement)?))
            }
            crate::ast::AstTerm::Application(lhs, rhs) => {
                let lhs = self.replace_term(*lhs, var.clone(), replacement.clone())?;
                let rhs = self.replace_term(*rhs, var, replacement)?;
                SyntaxTerm::Application(Box::new(lhs), Box::new(rhs))
            }
            crate::ast::AstTerm::Variable(v) => {
                let v_binding = self.resolve(&v)?;
                let var_binding = self.resolve(&var)?;
                if v_binding == var_binding {
                    replacement
                } else {
                    SyntaxTerm::Variable(v)
                }
            }
            crate::ast::AstTerm::Let(bind, val, body) => SyntaxTerm::Let(
                bind,
                Box::new(self.replace_term(*val, var.clone(), replacement.clone())?),
                Box::new(self.replace_term(*body, var, replacement)?),
            ),
            crate::ast::AstTerm::MacroDef(_, _) => term,
        })
    }

    fn replace_fragment(
        &mut self,
        fragment: SyntaxFragment,
        var: Syntax,
        replacement: SyntaxTerm,
    ) -> Result<SyntaxTerm> {
        match fragment {
            SyntaxFragment::Term(term) => self.replace_term(term, var, replacement),
        }
    }

    #[tracing::instrument]
    fn apply_macro(&mut self, def: MacroDef, rhs: SyntaxTerm) -> Result<SyntaxTerm> {
        self.replace_fragment(*def.body, def.arg, rhs)
        // def.body
    }

    fn expand_term_shallow(&mut self, term: SyntaxTerm) -> Result<Option<SyntaxTerm>> {
        Ok(Some(match term {
            SyntaxTerm::Application(lhs, rhs) => match *lhs {
                SyntaxTerm::Variable(var) => {
                    let binding = self.resolve(&var)?.unwrap();
                    if let Some(mac) = self.macros.get(&binding).cloned() {
                        self.apply_macro(mac, *rhs)?
                    } else {
                        SyntaxTerm::Application(Box::new(SyntaxTerm::Variable(var)), rhs)
                    }
                }
                SyntaxTerm::MacroDef(mut arg, mut body) => {
                    let scope = Scope::new();
                    arg.add_scope(scope.clone());
                    body.add_scope(scope);
                    self.apply_macro(MacroDef { arg, body }, *rhs)?
                }
                lhs => SyntaxTerm::Application(Box::new(lhs), rhs),
            },
            term => term,
        }))
    }

    #[tracing::instrument]
    pub fn expand_term(&mut self, term: SyntaxTerm) -> Result<Option<SyntaxTerm>> {
        Ok(Some(match term {
            SyntaxTerm::Abstraction(mut id, mut body) => {
                let scope = Scope::new();
                id.add_scope(scope.clone());
                body.add_scope(scope);
                self.add_binding(id.clone(), Binding::new());
                SyntaxTerm::Abstraction(id, Box::new(try_opt!(self.expand_term(*body)?)))
            }
            SyntaxTerm::Application(lhs, rhs) => match *lhs {
                SyntaxTerm::Variable(var) => {
                    let binding = self
                        .resolve(&var)?
                        .expect(&format!("failed to resolve {var:?}"));
                    if let Some(mac) = self.macros.get(&binding).cloned() {
                        self.apply_macro(mac, *rhs)?
                    } else {
                        SyntaxTerm::Application(Box::new(SyntaxTerm::Variable(var)), rhs)
                    }
                }
                lhs => {
                    let lhs = try_opt!(self.expand_term(lhs)?);
                    let rhs = try_opt!(self.expand_term(*rhs)?);
                    try_opt!(self.expand_term_shallow(SyntaxTerm::Application(
                        Box::new(lhs),
                        Box::new(rhs),
                    ))?)
                }
            },
            SyntaxTerm::Variable(var) => SyntaxTerm::Variable(var),
            SyntaxTerm::Let(mut bind, value, mut body) => {
                let scope = Scope::new();
                bind.add_scope(scope.clone());
                body.add_scope(scope);
                self.add_binding(bind.clone(), Binding::new());
                SyntaxTerm::Let(
                    bind,
                    Box::new(try_opt!(self.expand_term(*value)?)),
                    Box::new(try_opt!(self.expand_term(*body)?)),
                )
            }
            SyntaxTerm::MacroDef(mut arg, mut body) => {
                let scope = Scope::new();
                arg.add_scope(scope.clone());
                body.add_scope(scope);
                SyntaxTerm::MacroDef(arg, body)
            }
        }))
    }
}
