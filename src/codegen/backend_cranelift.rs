use ::cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_codegen::ir::{Signature, UserFuncName};
use cranelift_codegen::ir::AbiParam;
use crate::ast::path::AbsolutePath;

pub struct Context
{
    functions: ::std::collections::HashMap<AbsolutePath, (UserFuncName, Signature)>
}

impl Context
{
    pub fn declare_function(&mut self, path: AbsolutePath, args: &[crate::ast::Type], ret: &crate::ast::Type) {
        let sig = {
            let mut sig = cranelift_codegen::ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
            self.to_abi_params(&mut sig.returns, ret);
            for aty in args {
                self.to_abi_params(&mut sig.params, aty);
            }
            sig
            };
        let name = UserFuncName::user(0, self.functions.len() as u32);
        self.functions.insert(path, (name, sig));

    }
    pub fn lower_function(&mut self, name: &AbsolutePath, ir: &super::ir::Expr)
    {
        use cranelift_codegen::ir::InstBuilder;
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let (name,sig) = self.functions.get(name).unwrap();
        let mut func = ::cranelift_codegen::ir::Function::with_name_signature(name.clone(), sig.clone());
    
        let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

        let blocks: Vec<_> = (0 .. ir.blocks.len()).map(|_| builder.create_block()).collect();
        builder.append_block_params_for_function_params(blocks[0]);
        builder.switch_to_block(blocks[0]);
        builder.seal_block(blocks[0]);

        // NOTE: This needs to convert the non-SSA form into SSA
        // - This can be done by determining which locals are borrowed or written twice
        let alloca_override: Vec<_> = ir.locals.iter().map(|t| is_type_complex(t)).collect();

        for block in &ir.blocks {

            for stmt in &block.statements {
                use super::ir::Operation;
                match stmt {
                Operation::AssignLocal(local_index, value) => todo!(),
                Operation::AssignDeref(local_index, value) => todo!(),
                Operation::CreateComposite(local_index, absolute_path, values) => todo!(),
                Operation::CreateDataVariant(local_index, absolute_path, _, values) => todo!(),
                Operation::BinOp(local_index, value, bin_op, value1) => todo!(),
                Operation::UniOp(local_index, uni_op, value) => todo!(),
                Operation::BitShift(local_index, value, bit_shift, value1) => todo!(),
                Operation::BorrowLocal(local_index, _, local_index1, wrapper_list) => todo!(),
                Operation::BorrowGlobal(local_index, _, absolute_path, wrapper_list) => todo!(),
                Operation::PointerOffset(local_index, _, local_index1, wrapper_list) => todo!(),
                }
            }

            use super::ir::Terminator;
            match &block.terminator {
            Terminator::Unreachable => todo!(),
            Terminator::Goto(tgt) => {
                builder.ins().jump(blocks[tgt.index], &[]);
            },
            Terminator::Return(value) => todo!(),
            Terminator::Compare { lhs, op, rhs, if_true, if_false } => todo!(),
            Terminator::CallPath { dst, tgt, path, args } => todo!(),
            Terminator::MatchEnum { value, index, if_true, if_false  } => todo!(),
            Terminator::CallValue { dst, tgt, ptr, args } => todo!(),
            }
        }
    }

    pub fn to_abi_params(&self, dst: &mut Vec<AbiParam>, ty: &crate::ast::Type)
    {
        use cranelift_codegen::ir::types as t;
        use crate::ast::ty::{TypeKind,IntClass};
        let ptr = t::I64;
        match &ty.kind {
        TypeKind::Infer { .. } | TypeKind::TypeOf(..) => panic!("Unexpanded {:?}", ty),
        TypeKind::Void | TypeKind::UnsizedArray(..) => panic!("Unexpected {:?}", ty),

        TypeKind::Bool => dst.push(AbiParam::new(t::I8)),
        TypeKind::Integer(int_class) => dst.push(AbiParam::new(match int_class
            {
            IntClass::PtrInt|IntClass::PtrDiff => ptr,
            IntClass::Signed(shift)|IntClass::Unsigned(shift) => match shift
                {
                0 => t::I8,
                1 => t::I16,
                2 => t::I32,
                3 => t::I64,
                4 => t::I128,
                _ => panic!("Too-large integer type"),
                },
            })),
        TypeKind::Tuple(items) => {
            for ty in items {
                self.to_abi_params(dst, ty);
            }
        },
        TypeKind::Named(path) => todo!(),
        TypeKind::Pointer { .. } => dst.push(AbiParam::new(ptr)),
        TypeKind::Array { inner, count } => todo!(),
        }
    }

}


/// Is the passed type too big (or complex) to store in a single cranelift register
fn is_type_complex(t: &crate::ast::Type) -> bool {
    use crate::ast::ty::TypeKind;
    match &t.kind {
    TypeKind::Infer { .. }|TypeKind::TypeOf(..) => panic!("Unexpected {:?}", t),

    TypeKind::Void
    |TypeKind::Bool
    |TypeKind::Integer(..) => false,

    TypeKind::Pointer { .. } => false,

    TypeKind::Tuple(items) => {
        match &items[..] {
        [] => false,
        [t] => is_type_complex(t),
        _ => true,
        }
    },
    TypeKind::Named(type_path) => {
        use crate::ast::path::TypeBinding;
        let crate::ast::ty::TypePath::Resolved(b) = type_path else { panic!("Unbound {:?}", t); };
        match b {
        TypeBinding::Alias(_) => panic!("Unresolved alias {:?}", t),
        TypeBinding::EnumVariant(_, _) => todo!("Type bound to enum variant? {:?}", t),
        TypeBinding::Union(absolute_path) => todo!(),
        TypeBinding::Struct(absolute_path) => todo!(),
        TypeBinding::ValueEnum(_) => false,
        TypeBinding::DataEnum(_) => true,
        }
    },
    TypeKind::Array { inner, count } => {
        let &crate::ast::ty::ArraySize::Known(count) = count else { panic!("Unresolved type size: {:?}", t); };
        match count {
        0 => false,
        1 => is_type_complex(inner),
        _ => true,
        }
    },
    TypeKind::UnsizedArray(_) => todo!(),
    }
}