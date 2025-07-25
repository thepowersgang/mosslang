//! Apply the results of type checking/inference to the expression tree
// cspell:ignore ivars ivar
use crate::INDENT;
use crate::ast::ty::{Type,TypeKind};

/// Apply ivars to expression tree
pub fn commit_to_expr(ivars: &mut [super::ivars::IVarEnt], expr: &mut crate::ast::ExprRoot) {
    
    for (idx,iv) in ivars.iter_mut().enumerate() {
        if let TypeKind::Infer { index: None, .. } = iv.ty.kind {
            println!("{INDENT}commit_to_expr: _#{} = {:?}", idx, iv.cls);
            match iv.cls {
            super::ivars::InferType::None => {},    // Will error later?
            super::ivars::InferType::Integer => iv.ty = Type::new_integer(crate::Span::new_null(), crate::ast::ty::IntClass::Signed(2)),
            super::ivars::InferType::Float => todo!(), //iv.ty = Type::new_float(),
            super::ivars::InferType::Pointer => iv.ty = Type::new_ptr(crate::Span::new_null(), false, Type::new_void(crate::Span::new_null())),
            }
        }
        else {
            // Resolved
        }
    }

    let mut v = Visitor { ivars };
    for ty in &mut expr.variables {
        v.commit_ivars_in(&expr.e.span, ty);
    }
    crate::ast::visit_mut_expr(&mut v, &mut expr.e);
}

struct Visitor<'a> {
    ivars: &'a [super::ivars::IVarEnt],
}
impl<'a> Visitor<'a> {
    fn commit_ivars_in(&mut self, span: &crate::Span, ty: &mut Type) {
        let _i = INDENT.inc_f("commit_ivars_in", format_args!("{ty}"));
        match &mut ty.kind {
        TypeKind::Infer { .. } => {
            let new_ty = super::ivars::get_ivar(self.ivars, ty);
            match new_ty.kind {
            TypeKind::Infer { .. } => panic!("{span}: Type resolved to an ivar - {} -> {}", ty, new_ty),
            _ => {
                *ty = new_ty.clone();
                self.commit_ivars_in(span, ty);   // TODO: Somehow prevent infinite recursion
                },
            }
        },
        TypeKind::Void => {},
        TypeKind::Bool => {},
        TypeKind::Integer(..) => {},
        
        TypeKind::Named(..) => {},

        TypeKind::Tuple(items) => {
            for ty in items {
                self.commit_ivars_in(span, ty);
            }
        },
        TypeKind::Pointer { is_const: _, inner } => self.commit_ivars_in(span, inner),
        TypeKind::Array { inner, count: _ } => self.commit_ivars_in(span, inner),
        TypeKind::UnsizedArray(inner) => self.commit_ivars_in(span, inner),
        TypeKind::TypeOf(expr) => {
            crate::ast::ExprVisitor::visit_mut_expr(self, &mut expr.0.e);
            *ty = expr.0.e.data_ty.clone();
        },
        }
    }
}

impl<'a> crate::ast::ExprVisitor for Visitor<'a> {
    fn visit_mut_pattern(&mut self, pat: &mut crate::ast::Pattern, refutable: bool) {
        //for binding in &pat.bindings {
        //    self.binding.index.unwrap()
        //}
        use crate::ast::pattern::PatternTy;
        self.commit_ivars_in(&pat.span, &mut pat.data_ty);
        match &mut pat.ty {
        PatternTy::Any => {},
        PatternTy::Multiple(patterns) => {
            for pat in patterns {
                self.visit_mut_pattern(pat, refutable);
            }
        }
        PatternTy::MaybeBind(_) => {}
        PatternTy::ValueSingle(..) => {}
        PatternTy::ValueRangeIncl(..) => {}
        PatternTy::ValueRangeExcl(..) => {}
        PatternTy::Tuple(patterns) => {
            for pat in patterns {
                self.visit_mut_pattern(pat, refutable);
            }
        }
        }
    }
    
    fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
        let _i = INDENT.inc_f("commit: visit_expr", format_args!("{:?}", &expr.kind));
        self.commit_ivars_in(&expr.span, &mut expr.data_ty);
        use crate::ast::expr::ExprKind;
        match expr.kind {
        ExprKind::Cast(_, ref mut ty) => {
            self.commit_ivars_in(&expr.span, ty);
            },
        ExprKind::TypeInfoSizeOf(ref mut ty) => {
            self.commit_ivars_in(&expr.span, ty);
            }
        _ => {},
        }
        crate::ast::visit_mut_expr(self, expr);
        
        match expr.kind {
        ExprKind::Index(ref mut val_expr, _)
        |ExprKind::FieldIndex(ref mut val_expr, _)
        |ExprKind::FieldNamed(ref mut val_expr, _)
        => {
            // If indexing or accessing a field through a pointer, inject a Deref operation
            if let TypeKind::Pointer { is_const: _, ref inner } = val_expr.data_ty.kind {
                let ity = (**inner).clone();
                let sp = expr.span.clone();
                // Create a deref operation
                **val_expr = ExprKind::Deref(Box::new(::std::mem::replace(&mut **val_expr, ExprKind::Continue.to_expr(sp.clone())))).to_expr(sp);
                val_expr.data_ty = ity;
            }
            }
        _ => {},
        }
    }

    fn visit_mut_block(&mut self, block: &mut crate::ast::expr::Block) {
        for s in &mut block.statements {
            match s {
            crate::ast::expr::Statement::Expr(e) => self.visit_mut_expr(e),
            crate::ast::expr::Statement::Let(pattern, ty, expr) => {
                self.visit_mut_pattern(pattern, false);
                self.commit_ivars_in(&pattern.span, ty);    // HACK: Actually want the type span
                if let Some(expr) = expr {
                    self.visit_mut_expr(expr);
                }
            },
            }
        }
        if let Some(expr) = &mut block.result {
            self.visit_mut_expr(expr);
        }
    }
}