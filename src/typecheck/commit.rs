use crate::INDENT;
use crate::ast::ty::{Type,TypeKind};

pub fn commit_to_expr(ivars: &mut [super::ivars::IVarEnt], expr: &mut crate::ast::ExprRoot) {
    
    for (idx,iv) in ivars.iter_mut().enumerate() {
        if let TypeKind::Infer { index: None, .. } = iv.ty.kind {
            println!("{INDENT}commit_to_expr: _#{} = {:?}", idx, iv.cls);
            match iv.cls {
            super::ivars::InferType::None => {},    // Will error later?
            super::ivars::InferType::Integer => iv.ty = Type::new_integer(crate::ast::ty::IntClass::Signed(2)),
            super::ivars::InferType::Float => todo!(), //iv.ty = Type::new_float(),
            super::ivars::InferType::Pointer => iv.ty = Type::new_ptr(false, Type::new_void()),
            }
        }
        else {
            // Resolved
        }
    }

    let mut v = Visitor { ivars };
    crate::ast::visit_mut_expr(&mut v, &mut expr.e);
}

struct Visitor<'a> {
    ivars: &'a [super::ivars::IVarEnt],
}
impl<'a> Visitor<'a> {
    fn commit_ivars_in(&self, span: &crate::Span, ty: &mut Type) {
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
        TypeKind::NullPointer => {},
        
        TypeKind::Named(..) => {},

        TypeKind::Tuple(items) => {
            for ty in items {
                self.commit_ivars_in(span, ty);
            }
        },
        TypeKind::Pointer { is_const: _, inner } => self.commit_ivars_in(span, inner),
        TypeKind::Array { inner, count: _ } => self.commit_ivars_in(span, inner),
        }
    }
}

impl<'a> crate::ast::ExprVisitor for Visitor<'a> {
    fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
        let _i = INDENT.inc_f("commit: visit_expr", format_args!("{:?}", &expr.kind));
        self.commit_ivars_in(&expr.span, &mut expr.data_ty);
        match expr.kind {
        crate::ast::expr::ExprKind::Cast(_, ref mut ty) => {
            self.commit_ivars_in(&expr.span, ty);
            }
        _ => {},
        }
        crate::ast::visit_mut_expr(self, expr);
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