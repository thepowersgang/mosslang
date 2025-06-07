use crate::INDENT;
use ::cranelift_frontend::{FunctionBuilder, FunctionBuilderContext, Variable};
use cranelift_codegen::ir::{Signature, UserFuncName};
use cranelift_codegen::ir::AbiParam;
use crate::ast::path::AbsolutePath;
use crate::ast::ty::TypeKind;
use std::convert::TryFrom;

pub struct Context
{
    functions: ::std::collections::HashMap<AbsolutePath, (UserFuncName, Signature)>
}

impl Context
{
    pub fn new(output_path: &::std::path::Path) -> Context {
        Context {
            functions: Default::default(),
        }
    }

    /// Forward-declare a function, allowing Cranelift generation to know its signature
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
    /// Lower the body of a function
    pub fn lower_function(&mut self, state: &super::InnerState, name: &AbsolutePath, ir: &super::ir::Expr)
    {
        let _i = INDENT.inc_f("lower_function", format_args!("{}", name));
        use cranelift_codegen::ir::InstBuilder;
        let mut fn_builder_ctx = FunctionBuilderContext::new();
        let (name,sig) = self.functions.get(name).unwrap();
        let mut func = ::cranelift_codegen::ir::Function::with_name_signature(name.clone(), sig.clone());
    
        let mut builder = FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

        #[derive(Debug)]
        enum VariableValue {
            /// Not assigned yet
            Unassigned,
            /// ZST, never assign and return nothing.
            Empty,
            /// This is a stack slot
            StackSlot(cranelift_codegen::ir::StackSlot),
            Value(cranelift_codegen::ir::Value),
        }
        struct State<'a> {
            builder: &'a mut FunctionBuilder<'a>,
            blocks: Vec<cranelift_codegen::ir::Block>,
            variables: Vec<VariableValue>,
        }
        impl<'a> State<'a> {
            /// Obtain a cranelift `Value` from a moss IR `Value`, reading from memory or a stack slot if required
            fn read_value(&mut self, value: &crate::codegen::ir::Value) -> cranelift_codegen::ir::Value {
                use crate::codegen::ir::Value;
                match value {
                Value::Unreachable => todo!("Unreachable?"),
                Value::ImplicitUnit => self.builder.ins().iconst(cranelift_codegen::ir::Type::int(8).unwrap(), 0),
                Value::Local(local_index, wrapper_list) => {
                    if !wrapper_list.is_empty() {
                        match self.variables[local_index.0] {
                        VariableValue::StackSlot(stack_slot) => {
                            for w in wrapper_list.iter() {
                                todo!("Wrapper: {:?}", w);
                            }
                        },
                        ref s => todo!("Indirect access on non-slot: {local_index:?} : {:?}", s),
                        }
                    }
                    match self.variables[local_index.0] {
                    VariableValue::Unassigned => panic!("Unassigned slot? = #{}", local_index.0),
                    VariableValue::Empty => todo!("Empty value?"),
                    VariableValue::StackSlot(stack_slot) => todo!("stack"),
                    VariableValue::Value(value) => value,
                    }
                },
                Value::Named(absolute_path, wrapper_list) => todo!(),
                Value::Deref { ptr, wrappers } => todo!(),
                Value::StringLiteral(string_literal) => todo!(),
                Value::IntegerLiteral(value) =>
                    match u64::try_from(*value) {
                    Ok(v) => self.builder.ins().iconst(cranelift_codegen::ir::Type::int(64).unwrap(), v as i64),
                    Err(_) => todo!("big integer literal"),
                    },
                Value::FunctionPointer(absolute_path, function_pointer_ty) => todo!(),
                }
            }

            /// Store a read cranelift value into the specified local
            fn store_value(&mut self, local_index: &crate::codegen::ir::LocalIndex, value: cranelift_codegen::ir::Value) {
                if let VariableValue::StackSlot(ss) = self.variables[local_index.0] {
                    self.builder.ins().stack_store(value, ss, 0);
                }
                else {
                    self.variables[local_index.0] = VariableValue::Value(value);
                }
            }

            /// Helper to get the block and converted arguments for a jump target
            fn get_jump(&mut self, block: &crate::codegen::ir::JumpTarget) -> (cranelift_codegen::ir::Block, Vec<cranelift_codegen::ir::Value>) {
                (
                    cranelift_codegen::ir::Block::from_u32(block.index as u32),
                    block.args.iter().map(|l| self.read_value(&crate::codegen::ir::Value::Local(*l, Default::default()))).collect::<Vec<_>>()
                )
            }
        }
        let mut out_state = State {
            blocks: (0 .. ir.blocks.len()).map(|_| builder.create_block()).collect(),
            variables: ir.locals.iter().map(|_| VariableValue::Unassigned).collect(),
            builder: &mut builder,
        };

        for (i,ty) in ir.locals.iter().enumerate() {
            match ty.kind {
            TypeKind::Tuple(ref v) if v.is_empty() => {
                out_state.variables[i] = VariableValue::Empty;
            },
            _ if is_type_complex(ty) => {
                let ti = state.type_info(ty);
                out_state.variables[i] = VariableValue::StackSlot(out_state.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData {
                    kind: cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                    size: ti.size() as u32,
                    align_shift: ti.align().trailing_zeros() as u8,
                }));
            }
            _ => {},
            }
        }

        for (i,block) in ir.blocks.iter().enumerate() {
            if i == 0 {
                out_state.builder.append_block_params_for_function_params(out_state.blocks[0]);
                for (i,v) in out_state.builder.block_params(out_state.blocks[i]).iter().enumerate() {
                    if let VariableValue::Unassigned = out_state.variables[i] {
                        out_state.variables[i] = VariableValue::Value(*v);
                    }
                }
            }
            else {
                for p in block.args.iter() {
                    if let VariableValue::Unassigned = out_state.variables[p.0] {
                        let mut v = Vec::new();
                        self.to_abi_params(&mut v, &ir.locals[p.0]);
                        out_state.builder.append_block_param(out_state.blocks[i], v[0].value_type);
                    }
                }
                for (p,v) in Iterator::zip( block.args.iter(), out_state.builder.block_params(out_state.blocks[i]) ) {
                    if let VariableValue::Unassigned = out_state.variables[p.0] {
                        out_state.variables[p.0] = VariableValue::Value(*v);
                    }
                }
            }
        }

        // Pre-seal the first block
        out_state.builder.seal_block(out_state.blocks[0]);
        // Start lowering blocks
        for (block_idx,block) in ir.blocks.iter().enumerate() {
            out_state.builder.switch_to_block(out_state.blocks[block_idx]);
            println!("{INDENT}bb{}", block_idx);
            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                println!("{INDENT}BB{block_idx}/{stmt_idx}: {:?}", stmt);
                use super::ir::Operation;
                match stmt {
                Operation::Alloca { dst, ty } => {
                    let ti = state.type_info(ty);
                    out_state.variables[dst.0] = VariableValue::StackSlot(out_state.builder.create_sized_stack_slot(cranelift_codegen::ir::StackSlotData {
                        kind: cranelift_codegen::ir::StackSlotKind::ExplicitSlot,
                        size: ti.size() as u32,
                        align_shift: ti.align().trailing_zeros() as u8,
                    }));
                },
                Operation::AssignLocal(local_index, value) => {
                    let value = out_state.read_value(value);
                    out_state.store_value(local_index, value);
                },
                Operation::AssignDeref(local_index, value) => todo!("AssignDeref"),
                Operation::CreateComposite(local_index, absolute_path, values) => todo!(),
                Operation::CreateDataVariant(local_index, absolute_path, _, values) => todo!(),
                Operation::BinOp(local_index, value_r, bin_op, value_l) => {
                    use crate::codegen::ir::BinOp;
                    let x = out_state.read_value(value_l);
                    let y = out_state.read_value(value_r);
                    let v = match ir.locals[local_index.0].kind {
                        TypeKind::Integer(_) => match bin_op {
                            BinOp::Add => out_state.builder.ins().iadd(x, y),
                            BinOp::Sub => todo!(),
                            BinOp::Mul => todo!(),
                            BinOp::Div => todo!(),
                            BinOp::Rem => todo!(),
                            BinOp::BitOr => todo!(),
                            BinOp::BitAnd => todo!(),
                            BinOp::BitXor => todo!(),
                        },
                        _ => todo!("BinOp on {}", ir.locals[local_index.0]),
                    };
                    out_state.store_value(local_index, v);
                },
                Operation::UniOp(local_index, uni_op, value) => todo!(),
                Operation::BitShift(local_index, value, bit_shift, value1) => todo!(),
                Operation::BorrowLocal(local_index, _, local_index1, wrapper_list) => todo!(),
                Operation::BorrowGlobal(local_index, _, absolute_path, wrapper_list) => todo!(),
                Operation::PointerOffset(local_index, _, local_index1, wrapper_list) => todo!(),
                }
            }

            use super::ir::Terminator;
            println!("{INDENT}BB{block_idx}/T: {:?}", block.terminator);
            match &block.terminator {
            Terminator::Unreachable => todo!("terminator: Unreachable - is this even possible?"),
            Terminator::Goto(tgt) => {
                out_state.builder.ins().jump(out_state.blocks[tgt.index], &[]);
            },
            Terminator::Return(value) => todo!("terminator: Return"),
            Terminator::Compare { lhs, op, rhs, if_true, if_false } => {
                use crate::codegen::ir::CmpOp;
                use ::cranelift_codegen::ir::condcodes::IntCC;
                let cnd = match op {
                    CmpOp::Eq => IntCC::Equal,
                    CmpOp::Ne => IntCC::NotEqual,
                    CmpOp::Lt => todo!(),
                    CmpOp::Le => todo!(),
                    CmpOp::Gt => todo!(),
                    CmpOp::Ge => todo!(),
                };
                let x = out_state.read_value(lhs);
                let y = out_state.read_value(rhs);
                let cnd = out_state.builder.ins().icmp(cnd, x, y);
                let (then_label, then_args) = out_state.get_jump(if_true);
                let (else_label, else_args) = out_state.get_jump(if_false);
                out_state.builder.ins().brif( cnd, then_label, &then_args, else_label, &else_args);
            },
            Terminator::MatchEnum { value, index, if_true, if_false  } => todo!("terminator: Match"),
            Terminator::CallPath { dst, tgt, path, args } => todo!("terminator: CallPath"),
            Terminator::CallValue { dst, tgt, ptr, args } => todo!("terminator: CallValue"),
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
        // TODO: Structs and unions could be tagged with `#[repr(transparent)]` or otherwise only contain a single non-complex field, and thus fit in a register
        TypeBinding::Union(_absolute_path) => true,
        TypeBinding::Struct(_absolute_path) => true,
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