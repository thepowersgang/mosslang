//! Abstract Syntax Tree
//! 
//! A near 1:1 conversion of code into a tree

pub mod expr;
pub mod items;
pub mod ty;
pub mod path;

pub use self::expr::ExprRoot;
pub use self::ty::Type;
pub use self::path::Path;

pub struct Crate {
    pub attributes: Vec<Attribute>,
    pub module: items::Module,

    // TODO: Parsed crate attributes
}

pub type AbiSpec = Option<Vec<u8>>;

#[derive(Debug)]
pub struct Pattern {
    pub span: crate::Span,
    pub bindings: Vec<crate::Ident>,
    pub ty: PatternTy,
}
#[derive(Debug)]
pub enum PatternTy {
    Any,
    MaybeBind(crate::Ident),
    NamedValue(Path),
    Tuple(Vec<Pattern>),
}

#[derive(Debug)]
pub struct Attribute
{
    pub name: crate::Ident,
    pub data: AttributeData,
}
#[derive(Debug)]
pub enum AttributeData
{
    None,
    Value(Vec<u8>),
    SubItems(Vec<Attribute>),
}


pub trait ExprVisitor
{
    fn visit_mut_pattern(&mut self, pat: &mut Pattern, refutable: bool) { let _ = pat; let _ = refutable; }
    fn visit_mut_expr(&mut self, expr: &mut expr::Expr);
    fn visit_mut_block(&mut self, block: &mut expr::Block);
}
pub fn visit_mut_expr(c: &mut dyn ExprVisitor, expr: &mut crate::ast::expr::Expr)
{
    use crate::ast::expr::ExprKind;
    match &mut expr.kind {
    ExprKind::Block(block) => c.visit_mut_block(block),
    ExprKind::LiteralString(_)
    |ExprKind::LiteralInteger(_, _) => {},

    ExprKind::Return(expr)
    | ExprKind::Continue(expr)
    | ExprKind::Break(expr) => 
        if let Some(expr) = expr {
            c.visit_mut_expr(expr);
        },
    ExprKind::Assign { slot, op: _, value } => {
        c.visit_mut_expr(slot);
        c.visit_mut_expr(value);
    },
    ExprKind::NamedValue(_path) => {},
    ExprKind::CallPath(_path, exprs) => {
        for e in exprs {
            c.visit_mut_expr(e);
        }
    },
    ExprKind::Tuple(exprs) => {
        for e in exprs {
            c.visit_mut_expr(e);
        }
    },
    ExprKind::FieldNamed(expr, _) => c.visit_mut_expr(expr),
    ExprKind::FieldIndex(expr, _) => c.visit_mut_expr(expr),
    ExprKind::Index(expr_v, expr_i) => {
        c.visit_mut_expr(expr_v);
        c.visit_mut_expr(expr_i);
    },
    ExprKind::Addr(_, expr) => c.visit_mut_expr(expr),
    ExprKind::Deref(expr) => c.visit_mut_expr(expr),
    ExprKind::Cast(expr, _) => c.visit_mut_expr(expr),
    ExprKind::UniOp(_uni_op_ty, expr) => c.visit_mut_expr(expr),
    ExprKind::BinOp(_bin_op_ty, expr_l, expr_r) => {
        c.visit_mut_expr(expr_l);
        c.visit_mut_expr(expr_r);
    },
    ExprKind::CallValue(expr, exprs) => {
        c.visit_mut_expr(expr);
        for e in exprs {
            c.visit_mut_expr(e);
        }
    },
    ExprKind::Loop { body } => c.visit_mut_block(body),
    ExprKind::WhileLoop { cond, body, else_block } => {
        c.visit_mut_expr(cond);
        c.visit_mut_block(body);
        if let Some(block) = else_block {
            c.visit_mut_block(block);
        }
    },
    ExprKind::ForLoop { pattern, start, end, body, else_block } => {
        c.visit_mut_pattern(pattern, false);
        c.visit_mut_expr(start);
        c.visit_mut_expr(end);
        c.visit_mut_block(body);
        if let Some(block) = else_block {
            c.visit_mut_block(block);
        }
    },
    ExprKind::IfChain { branches, else_block } => {
        for b in branches {
            c.visit_mut_expr(&mut b.cond);
            c.visit_mut_block(&mut b.body);
        }
        if let Some(block) = else_block {
            c.visit_mut_block(block);
        }
    }
    ExprKind::Match { value, branches } => {
        c.visit_mut_expr(value);
        for b in branches {
            c.visit_mut_pattern(&mut b.pat, true);
            c.visit_mut_expr(&mut b.val);
        }
    }
    }
}
pub fn visit_mut_block(c: &mut dyn ExprVisitor, block: &mut crate::ast::expr::Block)
{
    for s in &mut block.statements {
        match s {
        crate::ast::expr::Statement::Expr(e) => c.visit_mut_expr(e),
        crate::ast::expr::Statement::Let(pattern, _, expr) => {
            c.visit_mut_pattern(pattern, false);
            if let Some(expr) = expr {
                c.visit_mut_expr(expr);
            }
        },
        }
    }
    if let Some(expr) = &mut block.result {
        c.visit_mut_expr(expr);
    }
}