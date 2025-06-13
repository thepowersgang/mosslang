use crate::INDENT;
use ::cranelift_codegen::ir::{self as cr_ir, AbiParam};
use super::ir as ms_ir;
use crate::ast::path::AbsolutePath;
use crate::ast::ty::TypeKind;
use std::convert::TryFrom;

pub struct Context
{
    module: ::cranelift_object::ObjectModule,
    functions: ::std::collections::HashMap<AbsolutePath, (cr_ir::UserFuncName, DeclaredValueItem)>,
    string_count: usize,
}
enum DeclaredValueItem {
    Function {
        sig: cr_ir::Signature,
    },
    ExternStatic,
}

impl Context
{
    pub fn new(output_path: &::std::path::Path, isa_name: &str) -> Context {
        let isa = {
			let shared_builder = ::cranelift_codegen::settings::builder();
			let shared_flags = ::cranelift_codegen::settings::Flags::new(shared_builder);
			let b = ::cranelift_codegen::isa::lookup_by_name(isa_name).unwrap();
			b.finish(shared_flags).expect("Failed to create TargetIsa")
			};
        let builder = ::cranelift_object::ObjectBuilder::new(
				isa,
				b"unknown_object.o"[..].to_owned(),
				::cranelift_module::default_libcall_names(),
				).expect("Can't create object builder");
        Context {
            module: ::cranelift_object::ObjectModule::new(builder),
            functions: Default::default(),
            string_count: 0,
        }
    }

    /// Forward-declare a function, allowing Cranelift generation to know its signature
    pub fn declare_function(&mut self, path: AbsolutePath, args: &[(crate::ast::Pattern, crate::ast::Type)], ret: &crate::ast::Type) {
        let sig = {
            let mut sig = cr_ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
            self.to_abi_params(&mut sig.returns, ret);
            for (_,aty) in args {
                self.to_abi_params(&mut sig.params, aty);
            }
            sig
            };
        let name = cr_ir::UserFuncName::user(0, self.functions.len() as u32);
        self.functions.insert(path, (name, DeclaredValueItem::Function { sig }));
    }
    /// Forward-declare a function, allowing Cranelift generation to know its signature
    pub fn declare_external_static(&mut self, path: AbsolutePath, _ty: &crate::ast::Type) {
        let name = cr_ir::UserFuncName::user(0, self.functions.len() as u32);
        self.functions.insert(path, (name, DeclaredValueItem::ExternStatic));
    }
    /// Lower the body of a function
    pub fn lower_function(&mut self, state: &super::InnerState, name: &AbsolutePath, ir: &super::ir::SsaExpr)
    {
        use cr_ir::InstBuilder;

        let ir = ir.get();
        let _i = INDENT.inc_f("lower_function", format_args!("{}", name));
        let mut fn_builder_ctx = ::cranelift_frontend::FunctionBuilderContext::new();
        let Some((name, DeclaredValueItem::Function { sig })) = self.functions.get(name) else { panic!("{} not defined as a function", name) };
        let mut func = ::cranelift_codegen::ir::Function::with_name_signature(name.clone(), sig.clone());
    
        let mut builder = ::cranelift_frontend::FunctionBuilder::new(&mut func, &mut fn_builder_ctx);

        #[derive(Debug)]
        enum VariableValue {
            /// Not assigned yet
            Unassigned,
            /// ZST, never assign and return nothing.
            Empty,
            /// This is a stack slot
            StackSlot(cr_ir::StackSlot),
            /// An assigned Cranelift value (a register)
            Value(cr_ir::Value),
        }
        enum ReadValue {
            Empty,
            Single(cr_ir::Value),
            //Multiple,
        }
        impl ReadValue {
            fn unwrap_single(self) -> cr_ir::Value {
                match self {
                ReadValue::Empty => panic!("Expected a single-register value, got empty"),
                ReadValue::Single(value) => value,
                }
            }
            fn into_iter(self) -> impl Iterator<Item=cr_ir::Value> {
                match self {
                ReadValue::Empty => None,
                ReadValue::Single(value) => Some(value),
                }.into_iter()
            }
        }
        struct State<'ir, 'a> {
            ctxt: &'ir mut Context,
            outer_state: &'ir super::InnerState<'ir>,
            ir: &'ir super::ir::Expr,
            builder: &'a mut ::cranelift_frontend::FunctionBuilder<'a>,
            blocks: Vec<cr_ir::Block>,
            variables: Vec<VariableValue>,
        }
        impl<'ir, 'a> State<'ir, 'a> {
            fn get_offset_from_wrappers<'ty>(&mut self, mut ty: &'ty crate::ast::Type, wrappers: &ms_ir::WrapperList) -> (cr_ir::Value, &'ty crate::ast::Type) {
                let mut rv = self.builder.ins().iconst(cr_ir::Type::int(64).unwrap(), 0);
                for w in wrappers.iter() {
                    use ms_ir::Wrapper;
                    match w {
                    Wrapper::Field(idx) => {
                        match &ty.kind {
                        TypeKind::Array { inner, count: _ } | TypeKind::UnsizedArray(inner) => {
                            let ti = self.outer_state.type_info(&inner);
                            let ofs = (idx * ti.size()) as i64;
                            rv = self.builder.ins().iadd_imm(rv, ofs);
                            ty = inner;
                        },
                        _ => todo!("Get field offset for {ty} #{}", idx),
                        }
                    },
                    Wrapper::IndexBySlot(local_index) => {
                        let (TypeKind::Array { inner, count: _ } | TypeKind::UnsizedArray(inner)) = &ty.kind else {
                            panic!("Indexing on invalid type: {}", ty);
                        };
                        let ti = self.outer_state.type_info(&inner);
                        let VariableValue::Value(idx) = self.variables[local_index.0] else { todo!("Indexing with other value types? {:?}", self.variables[local_index.0]); };
                        // rv = rv + idx * ti.size()
                        let ofs = self.builder.ins().imul_imm(idx, ti.size() as i64);
                        rv = self.builder.ins().iadd(rv, ofs);
                        ty = inner;
                    },
                    }
                }
                (rv, ty)
            }
            /// Obtain a cranelift `Value` from a moss IR `Value`, reading from memory or a stack slot if required
            fn read_value(&mut self, value: &ms_ir::Value) -> ReadValue {
                use ms_ir::Value;
                match value {
                Value::Unreachable => todo!("Unreachable?"),
                // TODO: Do nothing here?
                Value::ImplicitUnit => ReadValue::Empty,

                // To read directly from a value
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
                    VariableValue::Unassigned => panic!("Unassigned slot? _{}", local_index.0),
                    VariableValue::Empty => ReadValue::Empty,
                    VariableValue::StackSlot(stack_slot) => todo!("stack"),
                    VariableValue::Value(value) => ReadValue::Single(value),
                    }
                },
                Value::Named(absolute_path, wrapper_list) => {
                    let (ty,gv) = self.get_global(absolute_path);
                    if !wrapper_list.is_empty() {
                        todo!("Load global with wrappers")
                    }
                    else {
                        match get_types(ty)
                        {
                        TranslatedType::Empty => ReadValue::Empty,
                        TranslatedType::Complex => todo!("Global to complex type, will need to memcpy to destination"),
                        TranslatedType::Single(cr_ty) => ReadValue::Single(self.builder.ins().global_value(cr_ty, gv)),
                        }
                    }
                },
                Value::Deref { ptr, wrappers } => {
                    let TypeKind::Pointer { is_const: _, inner: ref val_ty } = self.ir.locals[ptr.0].kind else {
                        panic!("Deref on non-pointer: {ptr:?} - {ty}", ty=self.ir.locals[ptr.0]);
                    };
                    let ptr = match self.variables[ptr.0] {
                        VariableValue::Unassigned => panic!("Unassigned slot? _{}", ptr.0),
                        VariableValue::Empty => panic!("Pointer was an empty value, shouldn't be possible - IR generaton error?"),
                        VariableValue::StackSlot(_) => todo!("Load a pointer from a stack slot (why did a pointer end up assigned an alloca?)"),
                        VariableValue::Value(value) => value,
                        };
                    if !wrappers.is_empty() {
                        let (ofs, ty) = self.get_offset_from_wrappers(&val_ty, wrappers);
                        let ptr = self.builder.ins().iadd(ptr, ofs);
                        let mut flags = cr_ir::MemFlags::new();
                        if false {
                            flags.set_notrap();
                        }
                        match get_types(ty)
                        {
                        TranslatedType::Empty => todo!("Deref to empty type"),
                        TranslatedType::Complex => todo!("Deref to complex type, will need to memcpy to destination"),
                        TranslatedType::Single(cr_ty) => ReadValue::Single(self.builder.ins().load(cr_ty, flags, ptr, 0)),
                        }
                    }
                    else {
                        todo!("Read from pointer");
                    }
                },
                Value::StringLiteral(string_literal) => {
                    use cranelift_module::Module;
                    let string_name = format!("str#{}", self.ctxt.string_count);
                    self.ctxt.string_count += 1;
                    // Declare
                    let did = self.ctxt.module.declare_data(&string_name, ::cranelift_module::Linkage::Local, /*writeable*/false, /*tls*/false)
                        .expect("Failed to declare");
                    // Define
                    let mut data_ctx = ::cranelift_module::DataDescription::new();
                    data_ctx.define({ let mut val = string_literal.as_bytes().to_owned(); val.push(0); val.into_boxed_slice() });
                    self.ctxt.module.define_data(did, &data_ctx).expect("create_string - define_data");
                    // Use
                    let gv = self.ctxt.module.declare_data_in_func(did, self.builder.func);
                    ReadValue::Single(self.builder.ins().symbol_value( self.ctxt.ptr_ty(), gv ))
                },
                Value::IntegerLiteral(value) =>
                    match u64::try_from(*value) {
                    Ok(v) => ReadValue::Single(self.builder.ins().iconst(cr_ir::Type::int(64).unwrap(), v as i64)),
                    Err(_) => todo!("big integer literal"),
                    },
                Value::FunctionPointer(absolute_path, function_pointer_ty) => todo!(),
                }
            }

            /// Store a read cranelift value into the specified local
            fn store_value(&mut self, local_index: &ms_ir::LocalIndex, value: cr_ir::Value) {
                if let VariableValue::StackSlot(ss) = self.variables[local_index.0] {
                    self.builder.ins().stack_store(value, ss, 0);
                }
                else {
                    self.variables[local_index.0] = VariableValue::Value(value);
                }
            }

            /// Helper to get the block and converted arguments for a jump target
            fn get_jump(&mut self, block: &ms_ir::JumpTarget) -> (cr_ir::Block, Vec<cr_ir::Value>) {
                (
                    cr_ir::Block::from_u32(block.index as u32),
                    block.args.iter()
                        .flat_map(|l| self.read_value(&ms_ir::Value::Local(*l, Default::default())).into_iter())
                        .collect::<Vec<_>>()
                )
            }
            fn get_val_list(&mut self, values: &[ms_ir::Value]) -> Vec<cr_ir::Value> {
                values.iter()
                    .flat_map(|l| self.read_value(l).into_iter())
                    .collect::<Vec<_>>()
            }

            fn get_function(&mut self, path: &crate::ast::path::AbsolutePath) -> cr_ir::FuncRef {
                let Some((name, DeclaredValueItem::Function { sig })) = self.ctxt.functions.get(path) else { panic!("{} not defined as a function" ,path) };
                let name = self.builder.func.declare_imported_user_function(name.get_user().unwrap().clone());
                let signature = self.builder.import_signature(sig.clone());
                self.builder.import_function(cr_ir::ExtFuncData {
                    name: cr_ir::ExternalName::User(name),
                    signature,
                    colocated: false
                })
            }
            fn get_global(&mut self, path: &crate::ast::path::AbsolutePath) -> (&crate::ast::Type, cr_ir::GlobalValue) {
                let Some(ty) = self.outer_state.statics.get(path) else { panic!("Undefined static {}", path) };
                let Some((name, DeclaredValueItem::ExternStatic)) = self.ctxt.functions.get(path) else { panic!("{} not defined as a static",path) };
                let name = self.builder.func.declare_imported_user_function(name.get_user().unwrap().clone());
                let gv = self.builder.create_global_value(cr_ir::GlobalValueData::Symbol {
                    name: cr_ir::ExternalName::User(name),
                    offset: 0.into(),
                    colocated: false,
                    tls: false
                });
                (ty,gv)
            }
        }
        let mut out_state = State {
            ctxt: self,
            outer_state: state,
            ir,
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
                out_state.variables[i] = VariableValue::StackSlot(out_state.builder.create_sized_stack_slot(cr_ir::StackSlotData {
                    kind: cr_ir::StackSlotKind::ExplicitSlot,
                    size: ti.size() as u32,
                    align_shift: ti.align().trailing_zeros() as u8,
                }));
            }
            _ => {},
            }
        }

        // Define block parameters
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
                        match get_types(&ir.locals[p.0]) {
                        TranslatedType::Empty => todo!(),
                        TranslatedType::Single(cr_ty) => {
                            out_state.builder.append_block_param(out_state.blocks[i], cr_ty);
                        },
                        TranslatedType::Complex => todo!(),
                        }
                    }
                }
                for (p,v) in Iterator::zip( block.args.iter(), out_state.builder.block_params(out_state.blocks[i]) ) {
                    if let VariableValue::Unassigned = out_state.variables[p.0] {
                        out_state.variables[p.0] = VariableValue::Value(*v);
                    }
                }
            }
        }

        // Enumerate a visit order, so all variables are populated before they're used
        let block_visit_order = {
            let mut v = Vec::with_capacity(ir.blocks.len());
            let mut visited = crate::helpers::BitSet::new(ir.blocks.len());
            let mut stack = Vec::new();
            stack.push(0);
            while let Some(idx) = stack.pop() {
                if visited.set(idx) {
                    continue ;
                }
                v.push(idx);
                match &ir.blocks[idx].terminator {
                ms_ir::Terminator::Unreachable => {},
                ms_ir::Terminator::Return(_) => {},

                ms_ir::Terminator::Goto(tgt)
                |ms_ir::Terminator::CallPath { tgt, .. }
                |ms_ir::Terminator::CallValue { tgt, .. } => {
                    stack.push(tgt.index);
                },

                ms_ir::Terminator::Compare { if_true, if_false, .. }
                |ms_ir::Terminator::MatchEnum { if_true, if_false, .. } => {
                    stack.push(if_true.index);
                    stack.push(if_false.index);
                },
                }
            }
            v
        };

        // Pre-seal the first block
        out_state.builder.seal_block(out_state.blocks[0]);
        // Start lowering blocks
        // - NOTE: Visit the blocks in execution order, so variables are poulated before use
        //for (block_idx,block) in ir.blocks.iter().enumerate() {
        for block_idx in block_visit_order {
            let block = &ir.blocks[block_idx];
            out_state.builder.switch_to_block(out_state.blocks[block_idx]);
            println!("{INDENT}bb{}", block_idx);
            for (stmt_idx, stmt) in block.statements.iter().enumerate() {
                println!("{INDENT}BB{block_idx}/{stmt_idx}: {:?}", stmt);
                use ms_ir::Operation;
                match stmt {
                Operation::Alloca { dst, ty } => {
                    let ti = state.type_info(ty);
                    out_state.variables[dst.0] = VariableValue::StackSlot(out_state.builder.create_sized_stack_slot(cr_ir::StackSlotData {
                        kind: cr_ir::StackSlotKind::ExplicitSlot,
                        size: ti.size() as u32,
                        align_shift: ti.align().trailing_zeros() as u8,
                    }));
                },
                Operation::AssignLocal(local_index, value) => {
                    let value = out_state.read_value(value);
                    match value {
                    ReadValue::Empty => {},
                    ReadValue::Single(value) => out_state.store_value(local_index, value),
                    }
                },
                Operation::AssignDeref(local_index, value) => todo!("AssignDeref"),
                Operation::CreateComposite(local_index, absolute_path, values) => todo!(),
                Operation::CreateDataVariant(local_index, absolute_path, _, values) => todo!(),
                Operation::BinOp(local_index, value_r, bin_op, value_l) => {
                    use ms_ir::BinOp;
                    let x = out_state.read_value(value_l).unwrap_single();
                    let y = out_state.read_value(value_r).unwrap_single();
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

            use ms_ir::Terminator;
            println!("{INDENT}BB{block_idx}/T: {:?}", block.terminator);
            match &block.terminator {
            Terminator::Unreachable => todo!("terminator: Unreachable - is this even possible?"),
            Terminator::Goto(tgt) => {
                let (call_label, call_args) = out_state.get_jump(tgt);
                out_state.builder.ins().jump(call_label, &call_args);
            },
            Terminator::Return(value) => {
                let v = out_state.read_value(value);
                out_state.builder.ins().return_(&v.into_iter().collect::<Vec<_>>());
            },
            Terminator::Compare { lhs, op, rhs, if_true, if_false } => {
                use ms_ir::CmpOp;
                use cr_ir::condcodes::IntCC;
                let cnd = match op {
                    CmpOp::Eq => IntCC::Equal,
                    CmpOp::Ne => IntCC::NotEqual,
                    CmpOp::Lt => todo!(),
                    CmpOp::Le => todo!(),
                    CmpOp::Gt => todo!(),
                    CmpOp::Ge => todo!(),
                };
                let x = out_state.read_value(lhs).unwrap_single();
                let y = out_state.read_value(rhs).unwrap_single();
                let cnd = out_state.builder.ins().icmp(cnd, x, y);
                let (then_label, then_args) = out_state.get_jump(if_true);
                let (else_label, else_args) = out_state.get_jump(if_false);
                out_state.builder.ins().brif( cnd, then_label, &then_args, else_label, &else_args);
            },
            Terminator::MatchEnum { value, index, if_true, if_false  } => todo!("terminator: Match"),
            Terminator::CallPath { dst, tgt, path, args } => {
                let args = out_state.get_val_list(&args);
                let fr = out_state.get_function(path);
                let call_inst = out_state.builder.ins().call(fr, &args);
                let values = out_state.builder.inst_results(call_inst);
                match values {
                [] => {},
                [value] => out_state.store_value(dst, values[0]),
                [..] => todo!("Multiple returns?"),
                }

                let (call_label, call_args) = out_state.get_jump(tgt);
                out_state.builder.ins().jump(call_label, &call_args);
            },
            Terminator::CallValue { dst, tgt, ptr, args } => todo!("terminator: CallValue"),
            }
        }
    }

    pub fn to_abi_params(&self, dst: &mut Vec<AbiParam>, ty: &crate::ast::Type)
    {
        use cr_ir::types as t;
        use crate::ast::ty::{TypeKind,IntClass};
        let ptr = t::I64;
        match &ty.kind {
        TypeKind::Infer { .. } | TypeKind::TypeOf(..) => panic!("Unexpanded {:?}", ty),
        TypeKind::Void | TypeKind::UnsizedArray(..) => panic!("Unexpected {:?}", ty),

        TypeKind::Bool => dst.push(AbiParam::new(t::I8)),
        TypeKind::Integer(int_class) => dst.push(AbiParam::new(match int_class
            {
            IntClass::PtrInt|IntClass::PtrDiff => self.ptr_ty(),
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
        TypeKind::Pointer { .. } => dst.push(AbiParam::new(self.ptr_ty())),
        TypeKind::Array { inner, count } => todo!(),
        }
    }


    fn ptr_ty(&self) -> cr_ir::Type {
        cr_ir::types::I64
    }
}

enum TranslatedType {
    Empty,
    Single(cr_ir::Type),
    Complex,
}
fn get_types(ty: &crate::ast::Type) -> TranslatedType {
    use cr_ir::types as t;
    use crate::ast::ty::IntClass;
    let ptr = t::I64;
    match &ty.kind {
    TypeKind::Infer { .. } | TypeKind::TypeOf(..) => panic!("Unexpanded {:?}", ty),
    TypeKind::Void | TypeKind::UnsizedArray(..) => panic!("Unexpected {:?}", ty),

    TypeKind::Bool => TranslatedType::Single(t::I8),
    TypeKind::Integer(int_class) => TranslatedType::Single(match int_class
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
        }),
    TypeKind::Tuple(items) => match &items[..]
        {
        [] => TranslatedType::Empty,
        [ty] => get_types(ty),
        [..] => TranslatedType::Complex,
        },
    TypeKind::Named(type_path) => {
        use crate::ast::path::TypeBinding;
        let crate::ast::ty::TypePath::Resolved(b) = type_path else { panic!("Unbound named type {:?}", ty); };
        match b {
        TypeBinding::Alias(_) => panic!("Unresolved alias {:?}", ty),
        TypeBinding::EnumVariant(_, _) => todo!("Type bound to enum variant? {:?}", ty),
        // TODO: Structs and unions could be tagged with `#[repr(transparent)]` or otherwise only contain a single non-complex field, and thus fit in a register
        TypeBinding::Union(_absolute_path) => TranslatedType::Complex,
        TypeBinding::Struct(_absolute_path) => TranslatedType::Complex,
        TypeBinding::ValueEnum(_) => TranslatedType::Single(t::I32),
        TypeBinding::DataEnum(_) => TranslatedType::Complex,
        }
    },
    TypeKind::Pointer { .. } => TranslatedType::Single(ptr),
    TypeKind::Array { inner, count } => {
        let &crate::ast::ty::ArraySize::Known(count) = count else { panic!("Unresolved type size: {:?}", ty); };
        match count {
        0 => TranslatedType::Empty,
        1 => get_types(inner),
        _ => TranslatedType::Complex,
        }
    },
    }
}

/// Is the passed type too big (or complex) to store in a single cranelift register
fn is_type_complex(t: &crate::ast::Type) -> bool {
    if let TranslatedType::Complex = get_types(t) {
        true
    }
    else {
        false
    }
}