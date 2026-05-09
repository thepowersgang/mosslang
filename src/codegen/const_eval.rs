
// Issue notes:
// - Const Eval need types known, in order to create variables
//   - Types may need array const eval
//   - Workaround: Only allow primitive types (integers) in const eval


pub fn const_eval(crate_: &mut crate::ast::Crate)
{
    handle_module(&mut crate_.module);
}
fn handle_module(module: &mut crate::ast::items::Module) {
    for v in &mut module.items {
        use crate::ast::items::ItemType;
        match &mut v.ty {
        ItemType::Module(module) => handle_module(module),
        ItemType::Use(_) => {},
        ItemType::ExternCrate(_) => {},
        ItemType::ExternBlock(eb) => {
        },
        ItemType::TypeAlias(ty) => {
            handle_type(ty)
        },
        ItemType::Struct(str) => {
            for f in &mut str.fields {
                handle_type(&mut f.ty)
            }
        },
        ItemType::Enum(enm) => {
            for v in &mut enm.variants {
                match &mut v.ty {
                crate::ast::items::EnumVariantTy::Bare => {},
                crate::ast::items::EnumVariantTy::Value(expr_root) => handle_cv(expr_root),
                crate::ast::items::EnumVariantTy::Data(ty) => handle_type(ty),
                }
            }
        },
        ItemType::Union(u) => {
            for f in &mut u.variants {
                handle_type(&mut f.ty)
            }
        },
        ItemType::Function(function) => {
            handle_expr(&mut function.code);
        },
        ItemType::Static(s) => {
            handle_type(&mut s.ty);
            handle_cv(&mut s.value);
        },
        ItemType::Constant(i) => {
            handle_type(&mut i.ty);
            handle_cv(&mut i.value);
        },
        }
    }
}

fn handle_type(ty: &mut crate::ast::Type) {
    use crate::ast::ty::TypeKind;
    match &mut ty.kind {
    TypeKind::Infer { .. } => {},
    TypeKind::Void => {},
    TypeKind::Bool => {},
    TypeKind::Integer(_) => {},
    TypeKind::Tuple(items) => {
        for ty in items {
            handle_type(ty);
        }
    },
    TypeKind::Named(_) => {},
    TypeKind::Pointer { is_const: _, inner } => handle_type(inner),
    TypeKind::Array { inner, count } => {
        handle_type(inner);
        match count {
        crate::ast::ty::ArraySize::Unevaluated(expr_root) => {
            handle_expr(expr_root);
            let v = evaluate(expr_root);
            todo!();
        },
        crate::ast::ty::ArraySize::Known(_) => todo!(),
        }
    },
    TypeKind::UnsizedArray(inner) => handle_type(inner),
    TypeKind::TypeOf(_) => {},
    }
}

fn handle_cv(v: &mut crate::ast::items::ConstantValue) {
    match v {
    crate::ast::items::ConstantValue::Unknown(expr_root) => {
        handle_expr(expr_root);
        *v = crate::ast::items::ConstantValue::Evaluated(evaluate(expr_root))
    },
    crate::ast::items::ConstantValue::Evaluated(items) => {},
    }
}
fn handle_expr(_expr_root: &mut crate::ast::ExprRoot) {
}

use crate::ast::items::EvaluatedConstant;
fn evaluate(expr_root: &crate::ast::ExprRoot) -> EvaluatedConstant {
    let mut state = State {
    };
    return match state.eval_node(&expr_root.e) {
        Ok(v) => v,
        Err(e) => match e {
            BreakTarget::Continue|BreakTarget::Break(_) => panic!("break/continue from non-loop"),
            BreakTarget::Return(evaluated_value) => evaluated_value,
            }
        };

    enum BreakTarget {
        Continue,
        Break(EvaluatedConstant),
        Return(EvaluatedConstant),
    }
    struct State {
    }
    impl State {
        fn alloc(&self, ty: &crate::ast::Type) -> EvaluatedConstant {
            let pointer_size = 8;
            let s = match &ty.kind {
                crate::ast::ty::TypeKind::Infer { .. } => panic!("Infer after typecheck"),
                crate::ast::ty::TypeKind::Void => panic!("Void type seen"),
                crate::ast::ty::TypeKind::Bool => 1,
                crate::ast::ty::TypeKind::Integer(int_class) => match int_class {
                    crate::ast::ty::IntClass::PtrInt
                    |crate::ast::ty::IntClass::PtrDiff => pointer_size,
                    crate::ast::ty::IntClass::Signed(s)
                    |crate::ast::ty::IntClass::Unsigned(s) => 1 << s,
                    },
                crate::ast::ty::TypeKind::Tuple(items) => {
                    if items.is_empty() {
                        0
                    }
                    else {
                        todo!()
                    }
                },
                crate::ast::ty::TypeKind::Named(type_path) => todo!(),
                crate::ast::ty::TypeKind::Pointer { is_const, inner } => pointer_size,
                crate::ast::ty::TypeKind::Array { inner, count } => todo!(),
                crate::ast::ty::TypeKind::UnsizedArray(_) => panic!("allocating unsized array"),
                crate::ast::ty::TypeKind::TypeOf(_) => panic!("TypeOf after typecheck"),
                };
            EvaluatedConstant(vec![0; s])
        }
        fn integer_inc(&self, v: &mut EvaluatedConstant) {
            // Little endian
            for v in &mut v.0 {
                *v = v.wrapping_add(1);
                if *v != 0 {
                    break;
                }
            }
        }
        fn eval_block(&mut self, block: &crate::ast::expr::Block) -> Result<EvaluatedConstant,BreakTarget> {
            for s in &block.statements {
                match s {
                crate::ast::expr::Statement::Expr(expr) => { let _ = self.eval_node(expr)?; },
                crate::ast::expr::Statement::Let(pattern, _, expr) => todo!(),
                }
            }
            match &block.result {
            Some(node) => self.eval_node(node),
            None => Ok(EvaluatedConstant(Vec::new())),
            }
        }
        fn eval_node(&mut self, expr: &crate::ast::expr::Expr) -> Result<EvaluatedConstant,BreakTarget> {
            use crate::ast::expr::ExprKind;
            Ok(match &expr.kind {
            ExprKind::Block(block) => self.eval_block(block)?,
            ExprKind::LiteralString(string_literal) => todo!(),
            ExprKind::LiteralInteger(v, int_lit_class) => {
                let mut rv = self.alloc(&expr.data_ty);
                match int_lit_class {
                crate::ast::expr::IntLitClass::Pointer => todo!(),
                _ => {
                    let crate::ast::ty::TypeKind::Integer(i) = expr.data_ty.kind else { panic!(); };
                    match i {
                    crate::ast::ty::IntClass::PtrInt => todo!(),
                    crate::ast::ty::IntClass::PtrDiff => todo!(),
                    crate::ast::ty::IntClass::Signed(s)
                    |crate::ast::ty::IntClass::Unsigned(s) => {
                        rv.0.copy_from_slice(&v.to_le_bytes()[..1 << s]);
                    }
                    }
                    },
                }
                rv
            },
            ExprKind::LiteralBoolean(v) => {
                let mut rv = self.alloc(&expr.data_ty);
                rv.0[0] = *v as u8;
                rv
            },
            ExprKind::TypeInfoSizeOf(_) => todo!(),
            ExprKind::Return(expr) => return Err(BreakTarget::Return(match expr
                {
                Some(expr) => self.eval_node(expr)?,
                None => EvaluatedConstant(Vec::new()),
                })),
            ExprKind::Continue => return Err(BreakTarget::Continue),
            ExprKind::Break(expr) => return Err(BreakTarget::Break(match expr
                {
                Some(expr) => self.eval_node(expr)?,
                None => EvaluatedConstant(Vec::new()),
                })),
            ExprKind::Assign { slot, op, value } => todo!(),
            ExprKind::NamedValue(path, value_binding) => todo!(),
            ExprKind::CallPath(path, value_binding, exprs) => todo!(),
            ExprKind::Tuple(exprs) => todo!(),
            ExprKind::Struct(path, type_binding, items) => todo!(),
            ExprKind::FieldNamed(expr, ident) => todo!(),
            ExprKind::FieldIndex(expr, _) => todo!(),
            ExprKind::Index(expr, expr1) => todo!(),
            ExprKind::Addr(_, expr) => todo!(),
            ExprKind::Deref(expr) => todo!(),
            ExprKind::Cast(expr, _) => todo!(),
            ExprKind::Coerce(expr) => todo!(),
            ExprKind::UniOp(uni_op_ty, expr) => {
                let mut v = self.eval_node(expr)?;
                match uni_op_ty {
                crate::ast::expr::UniOpTy::Invert => {
                    for v in &mut v.0 {
                        *v = !*v;
                    }
                    v
                },
                crate::ast::expr::UniOpTy::Negate => {
                    for v in &mut v.0 {
                        *v = !*v;
                    }
                    self.integer_inc(&mut v);
                    v
                },
                }
            },
            ExprKind::BinOp(bin_op_ty, expr, expr1) => todo!(),
            ExprKind::CallValue(expr, exprs) => todo!(),
            ExprKind::Loop { body } => {
                loop {
                    match self.eval_block(body)
                    {
                    Ok(_) => {},
                    Err(BreakTarget::Break(v)) => return Ok(v),
                    Err(BreakTarget::Continue) => continue,
                    Err(BreakTarget::Return(v)) => return Err(BreakTarget::Break(v)),
                    }
                }
            },
            ExprKind::WhileLoop { cond, body, else_block } => todo!(),
            ExprKind::ForLoop { pattern, start, end, body, else_block } => todo!(),
            ExprKind::IfChain { branches, else_block } => todo!(),
            ExprKind::Match { value, branches } => todo!(),
            })
        }
    }
}
