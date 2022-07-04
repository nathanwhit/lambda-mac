use std::{fmt::Debug, mem, ops::Not, sync::atomic::AtomicU64};

use crate::{
    ast::Stmt,
    syntax::{
        HasSyntax, IntoDatum, IntoSyntax, Scope, Syntax, SyntaxFragment, SyntaxStmt, SyntaxTerm,
    },
};
use color_eyre::Result;
use im::HashMap;
use tracing::instrument;

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

    pub fn expand(&mut self) -> Result<Vec<Stmt>> {
        let program = mem::take(&mut self.program);
        let mut output = Vec::new();
        for stmt in program {
            tracing::debug!(?stmt, "expanding statment");
            let result = self.context.expand_stmt(stmt)?;
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

impl Default for ExpansionContext {
    fn default() -> Self {
        Self::new()
    }
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
    #[instrument(skip(self))]
    fn add_binding(&mut self, id: Syntax, binding: Binding) {
        self.bindings.insert(id, binding);
    }
    #[instrument(ret)]
    fn resolve(&self, var: &Syntax) -> Result<Option<Binding>> {
        let candidates: Vec<_> = self
            .bindings
            .keys()
            .filter(|&can| {
                tracing::trace!(?can, "finding");
                dbg!(can.ident() == var.ident()) && dbg!(can.subset_of(&var))
            })
            .collect();
        tracing::trace!(?candidates, "Got candidates");
        let best = if let Some(&best) = candidates.iter().max_by_key(|&&k| k.num_scopes()) {
            best
        } else {
            return Ok(None);
        };

        if candidates
            .into_iter()
            .any(|cand| cand.subset_of(best).not())
        {
            return Err(color_eyre::eyre::eyre!("ambiguous: {var:?}"));
        }

        Ok(Some(
            self.bindings
                .get(best)
                .expect("we just got it from the binding table!")
                .clone(),
        ))
    }

    #[instrument(skip(self))]
    pub fn expand_stmt(&mut self, mut stmt: SyntaxStmt) -> Result<Option<SyntaxStmt>> {
        self.apply_global_scopes(&mut stmt);
        Ok(Some(match stmt {
            crate::ast::AstStmt::Expr(e) => SyntaxStmt::Expr(try_opt!(self.expand_term(e)?)),
            crate::ast::AstStmt::Bind(mut id, SyntaxTerm::MacroDef(mut arg, mut body)) => {
                tracing::debug!("here");
                let scope = Scope::new();
                let mac_bind = Binding::new();
                id.add_scope(scope.clone());
                self.add_global_scope(scope);
                self.add_binding(id.clone(), mac_bind.clone());

                let scope = Scope::new();
                arg.add_scope(scope.clone());
                body.add_scope(scope);
                self.add_binding(arg.clone(), Binding::new());

                self.macros.insert(mac_bind, MacroDef { arg, body });

                return Ok(None);
            }
            crate::ast::AstStmt::Bind(mut id, value) => {
                let scope = Scope::new();
                id.add_scope(scope.clone());
                self.add_binding(id.clone(), Binding::new());
                self.add_global_scope(scope);

                SyntaxStmt::Bind(id, try_opt!(self.expand_term(value)?))
            }
            crate::ast::AstStmt::Import(path) => SyntaxStmt::Import(path),
        }))
    }

    fn variable_eq(&self, var1: &Syntax, var2: &Syntax) -> Result<bool> {
        Ok(self.resolve(var1)? == self.resolve(var2)?)
    }

    #[instrument(skip(self))]
    fn replace_term(
        &mut self,
        mut term: SyntaxTerm,
        var: Syntax,
        replacement: SyntaxTerm,
    ) -> Result<SyntaxTerm> {
        self.apply_global_scopes(&mut term);
        Ok(match term {
            crate::ast::AstTerm::Abstraction(arg, body) => {
                // TODO: clean this up
                let arg = if self.variable_eq(&arg, &var)? {
                    if let SyntaxTerm::Variable(v) = replacement.clone() {
                        tracing::debug!(?v, "replacing arg");
                        v
                    } else {
                        arg
                    }
                } else {
                    arg
                };
                SyntaxTerm::Abstraction(arg, Box::new(self.replace_term(*body, var, replacement)?))
            }
            crate::ast::AstTerm::Application(lhs, rhs) => {
                let lhs = self.replace_term(*lhs, var.clone(), replacement.clone())?;
                let rhs = self.replace_term(*rhs, var, replacement)?;
                SyntaxTerm::Application(Box::new(lhs), Box::new(rhs))
            }
            crate::ast::AstTerm::Variable(v) => {
                if self.variable_eq(&v, &var)? {
                    tracing::debug!(?replacement, "replacing variable");
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

    #[instrument(skip(self))]
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

    #[instrument(skip(self))]
    fn apply_macro(&mut self, def: MacroDef, mut rhs: SyntaxTerm) -> Result<SyntaxTerm> {
        let intro_scope = Scope::new();
        rhs.add_scope(intro_scope.clone());
        let mut replaced = self.replace_fragment(*def.body, def.arg, rhs)?;
        replaced.flip_scope(intro_scope);
        Ok(replaced)
    }

    #[instrument(skip(self))]
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
                    self.add_binding(arg.clone(), Binding::new());
                    self.apply_macro(MacroDef { arg, body }, *rhs)?
                }
                lhs => SyntaxTerm::Application(Box::new(lhs), rhs),
            },
            term => term,
        }))
    }

    #[instrument(skip(self))]
    pub fn expand_term(&mut self, term: SyntaxTerm) -> Result<Option<SyntaxTerm>> {
        Ok(Some(match term {
            SyntaxTerm::Abstraction(mut id, mut body) => {
                tracing::debug!("expanding abstraction");
                let scope = Scope::new();
                id.add_scope(scope.clone());
                body.add_scope(scope);
                self.add_binding(id.clone(), Binding::new());
                SyntaxTerm::Abstraction(id, Box::new(try_opt!(self.expand_term(*body)?)))
            }
            SyntaxTerm::Application(lhs, rhs) => match *lhs {
                SyntaxTerm::Variable(var) => {
                    tracing::debug!(?var, "expanding application of variable");
                    let binding = self
                        .resolve(&var)?
                        .expect(&format!("failed to resolve {var:?}"));
                    if let Some(mac) = self.macros.get(&binding).cloned() {
                        tracing::debug!("its a macro, applying");
                        self.apply_macro(mac, *rhs)?
                    } else {
                        tracing::debug!("non macro application");
                        SyntaxTerm::Application(Box::new(SyntaxTerm::Variable(var)), rhs)
                    }
                }
                lhs => {
                    tracing::debug!("expanding application");
                    let lhs = try_opt!(self.expand_term(lhs)?);
                    let rhs = try_opt!(self.expand_term(*rhs)?);
                    tracing::trace!(%lhs, %rhs, "expanded application");
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
                self.add_binding(arg.clone(), Binding::new());
                SyntaxTerm::MacroDef(arg, body)
            }
        }))
    }
}

#[cfg(test)]
mod test {
    use crate::parse::program;

    use super::*;
    use maplit::btreeset as s;
    use tracing::dispatcher::DefaultGuard;
    use tracing_subscriber::{prelude::*, EnvFilter, Registry};
    use tracing_tree::HierarchicalLayer;

    #[derive(Clone)]
    struct Var {
        syntax: Syntax,
        binding: Binding,
    }

    impl From<u64> for Scope {
        fn from(id: u64) -> Self {
            Scope::with_id(id)
        }
    }

    fn syntax<I, S>(id: &str, scopes: I) -> Syntax
    where
        I: IntoIterator<Item = S>,
        S: Into<Scope>,
    {
        Syntax::new(id.into(), scopes.into_iter().map(Into::into).collect())
    }

    fn bind() -> Binding {
        Binding::new()
    }

    impl ExpansionContext {
        fn add_var(&mut self, var: Var) {
            self.add_binding(var.syntax, var.binding)
        }

        fn add_vars(&mut self, vars: impl IntoIterator<Item = Var>) {
            for var in vars {
                self.add_var(var);
            }
        }
    }

    fn var<I, S>(id: &str, scopes: I) -> Var
    where
        I: IntoIterator<Item = S>,
        S: Into<Scope>,
    {
        Var {
            syntax: syntax(id, scopes),
            binding: bind(),
        }
    }

    impl ExpansionContext {
        fn with_vars(vars: impl IntoIterator<Item = Var>) -> Self {
            let mut ctx = ExpansionContext::new();
            ctx.add_vars(vars);
            ctx
        }
    }

    #[allow(dead_code)]
    fn init_tracing() -> DefaultGuard {
        let subscriber = Registry::default()
            .with(EnvFilter::from_env("RUST_LOG"))
            .with(HierarchicalLayer::new(2).with_indent_lines(true));
        tracing::subscriber::set_default(subscriber)
    }

    #[test]
    fn resolve_works() {
        let _guard = init_tracing();

        let a = var("a", s! { 0, 1, 2 });
        let b = var("b", s! { 0, 1, 2 });
        let a_2 = var("a", s! { 0, 1, 2, 3 });

        let context = ExpansionContext::with_vars([a.clone(), b.clone(), a_2.clone()]);

        let new_a = syntax("a", s! { 0, 1, 2, 3, 4 });
        assert_eq!(context.resolve(&new_a).unwrap().unwrap(), a_2.binding);
    }

    #[test]
    fn expand_term_works() {
        let input = "λx. x";
        let mut expander = Expander::new(program(input).unwrap().1);
        expander.expand().unwrap();
    }
}
