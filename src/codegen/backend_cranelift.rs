use crate::INDENT;
use ::cranelift_codegen::ir::{self as cr_ir, AbiParam};
use super::ir as ms_ir;
use crate::ast::path::AbsolutePath;
use crate::ast::ty::TypeKind;
use std::convert::TryFrom;

use cr_ir::InstBuilder as _;

pub struct Context
{
    ofp: ::std::fs::File,
    module: ::cranelift_object::ObjectModule,
    functions: ::std::collections::HashMap<AbsolutePath, (cr_ir::UserFuncName, DeclaredValueItem)>,
    string_count: usize,
}
enum DeclaredValueItem {
    Function {
        sig: cr_ir::Signature,
        indirect_return: bool,
        variadic_after: Option<usize>,
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
				output_path.file_name().unwrap().as_encoded_bytes().to_owned(),//b"unknown_object.o"[..].to_owned(),
				::cranelift_module::default_libcall_names(),
				).expect("Can't create object builder");
        Context {
            ofp: ::std::fs::File::create(output_path).unwrap(),
            module: ::cranelift_object::ObjectModule::new(builder),
            functions: Default::default(),
            string_count: 0,
        }
    }
    pub fn finalise(mut self) -> Result<(),::std::io::Error> {
        use std::io::Write;
        self.ofp.write_all(&self.module.finish().emit().expect("Error emitting"))?;
        Ok( () )
    }

    /// Forward-declare a function, allowing Cranelift generation to know its signature
    pub fn declare_function(&mut self, state: &super::InnerState, path: AbsolutePath, fcn_sig: &crate::ast::items::FunctionSignature) {
        let indirect_return;
        let sig = {
            let mut sig = cr_ir::Signature::new(cranelift_codegen::isa::CallConv::SystemV);
            let ret_ti = state.type_info(&fcn_sig.ret);
            indirect_return = !ret_ti.is_primitive_like() && ret_ti.size() > 0;
            if indirect_return {
                sig.params.push(AbiParam::new(self.ptr_ty()));
            }
            else {
                self.to_abi_params(state, &mut sig.returns, &fcn_sig.ret);
            }
            for (_,aty) in &fcn_sig.args {
                self.to_abi_params(state, &mut sig.params, aty);
            }
            sig
            };
        let name = cr_ir::UserFuncName::user(0, self.functions.len() as u32);
        let variadic_after = fcn_sig.is_variadic.then_some(fcn_sig.args.len());
        self.functions.insert(path, (name, DeclaredValueItem::Function { sig, indirect_return, variadic_after }));
    }
    /// Forward-declare a function, allowing Cranelift generation to know its signature
    pub fn declare_external_static(&mut self, _state: &super::InnerState, path: AbsolutePath, _ty: &crate::ast::Type) {
        let name = cr_ir::UserFuncName::user(0, self.functions.len() as u32);
        self.functions.insert(path, (name, DeclaredValueItem::ExternStatic));
    }
    /// Lower the body of a function
    pub fn lower_function(&mut self, state: &super::InnerState, path: &AbsolutePath, ir: &super::ir::SsaExpr)
    {
        let ir = ir.get();
        let _i = INDENT.inc_f("lower_function", format_args!("{}", path));
        let mut fn_builder_ctx = ::cranelift_frontend::FunctionBuilderContext::new();
        let mangled_name = {
            use std::fmt::Write;
            let mut rv = String::new();
            let _ = write!(&mut rv, "_ZM");
            for v in &path.0 {
                let _ = write!(&mut rv, "{}{}", v.len(), v);
            }
            rv
        };
        let Some(&(ref name, DeclaredValueItem::Function { ref sig, indirect_return, variadic_after: _ })) = self.functions.get(path) else { panic!("{} not defined as a function", path) };
        let mut func = ::cranelift_codegen::ir::Function::with_name_signature(name.clone(), sig.clone());
    
        let mut builder = ::cranelift_frontend::FunctionBuilder::new(&mut func, &mut fn_builder_ctx);
        lower_function(&mut builder, self, state, ir, indirect_return);
        builder.finalize();
        
		let func_id = self.module.declare_function(&mangled_name, cranelift_module::Linkage::Export, &func.signature).unwrap();

		let mut c = ::cranelift_codegen::Context::new();
		c.func = func;
        use cranelift_module::Module;
		match self.module.define_function(func_id, &mut c)
		{
		Ok(_) => {},
		Err(::cranelift_module::ModuleError::Compilation(e)) => match e
			{
			::cranelift_codegen::CodegenError::Verifier(errors) => {
				println!("{}", c.func.display());
				panic!("Failed to define function (verifier errors):\n{}", errors);
				},
			e => panic!("Failed to define function code: {:?}", e),
			},
		Err(e) => panic!("Failed to define function code: {:?}", e),
		}
    }

    pub fn to_abi_params(&self, state: &super::InnerState, dst: &mut Vec<AbiParam>, ty: &crate::ast::Type)
    {
        use cr_ir::types as t;
        use crate::ast::ty::{TypeKind,IntClass};
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
                self.to_abi_params(state, dst, ty);
            }
        },
        TypeKind::Named(..) => {
            let ti = state.type_info(ty);
            if ti.is_primitive_like() {
                dst.push(AbiParam::new(match ti.size().trailing_zeros() {
                    0 => t::I8,
                    1 => t::I16,
                    2 => t::I32,
                    3 => t::I64,
                    4 => t::I128,
                    _ => panic!("Too-large integer type"),
                    }));
            }
            else {
                todo!("to_abi_params: {:?}", ty)
            }
        },
        TypeKind::Pointer { .. } => dst.push(AbiParam::new(self.ptr_ty())),
        TypeKind::Array { inner, count } => todo!("to_abi_params: {:?}", ty),
        }
    }


    fn ptr_ty(&self) -> cr_ir::Type {
        cr_ir::types::I64
    }
}

/// The core logic for converting between IRs
fn lower_function<'l1, 'l2>(builder: &'l1 mut cranelift_frontend::FunctionBuilder<'l2>, ctxt: &mut Context, outer_state: &super::InnerState, ir: &ms_ir::Expr, indirect_return: bool)
where
    'l2: 'l1
{
    let mut out_state = State {
        ctxt,
        outer_state,
        ir,
        blocks: (0 .. ir.blocks.len()).map(|_| builder.create_block()).collect(),
        variables: ir.locals.iter().map(|_| VariableValue::Unassigned).collect(),
        builder,
    };

    for (i,ty) in ir.locals.iter().enumerate() {
        match ty.kind {
        TypeKind::Void => {
            out_state.variables[i] = VariableValue::Empty;
            //panic!("Unexpected void local _{}", i);
        },
        TypeKind::Tuple(ref v) if v.is_empty() => {
            out_state.variables[i] = VariableValue::Empty;
        },
        _ if is_type_complex(ty) => {
            let ti = outer_state.type_info(ty);
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
    let mut return_ptr = None;
    for (i,block) in ir.blocks.iter().enumerate() {
        if i == 0 {
            out_state.builder.append_block_params_for_function_params(out_state.blocks[0]);
            let mut it = out_state.builder.block_params(out_state.blocks[i]).iter();
            if indirect_return {
                return_ptr = it.next().copied();
            }
            for (i,v) in it.enumerate() {
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
    let return_ptr = return_ptr;

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
    // - NOTE: Visit the blocks in approximate execution order, so variables are poulated before use
    for block_idx in block_visit_order {
        visit_block(&mut out_state, return_ptr, block_idx);
    }

    out_state.builder.seal_all_blocks();
}

/// Convert a single block
fn visit_block(out_state: &mut State, return_ptr: Option<cr_ir::Value>, block_idx: usize)
{
    let ir = out_state.ir;
    
    let block = &ir.blocks[block_idx];
    out_state.builder.switch_to_block(out_state.blocks[block_idx]);
    println!("{INDENT}bb{}", block_idx);
    for (stmt_idx, stmt) in block.statements.iter().enumerate() {
        println!("{INDENT}BB{block_idx}/{stmt_idx}: {:?}", stmt);
        use ms_ir::Operation;
        match stmt {
        Operation::Alloca { dst, ty } => {
            let ti = out_state.outer_state.type_info(ty);
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
            ReadValue::Stack(src_ss, ty, src_ofs) => {
                let ti = out_state.outer_state.type_info(&ty);
                if let VariableValue::StackSlot(dst_ss) = out_state.variables[local_index.0] {
                    // TODO: only do the below if the type isn't a primtive (e.g. borrowed)
                    for i in 0 .. ti.size() / 4 {
                        let value = out_state.builder.ins().stack_load(cr_ir::types::I32, src_ss, (src_ofs + i * 4) as i32);
                        out_state.builder.ins().stack_store(value, dst_ss, (i * 4) as i32);
                    }
                    let ofs = ti.size() - ti.size() % 4;
                    for i in 0 .. ti.size() % 4 {
                        let value = out_state.builder.ins().stack_load(cr_ir::types::I8, src_ss, (src_ofs + ofs + i) as i32);
                        out_state.builder.ins().stack_store(value, dst_ss, (ofs + i) as i32);
                    }
                }
                else {
                    todo!("Assign from a stack slot to non-slot")
                }
            },
            }
        },
        Operation::AssignDeref(dst_ptr, value) => {
            let ptr = out_state.read_value_single(&ms_ir::Value::Local(*dst_ptr, Default::default()));
            let value = out_state.read_value(value);
            let flags = cr_ir::MemFlags::new();
            match value {
            ReadValue::Empty => {},
            ReadValue::Single(value) => {
                out_state.builder.ins().store(flags, value, ptr, 0);
            },
            ReadValue::Stack(src_ss, ty, src_ofs) => {
                todo!("AssignDeref - StackSlot value");
                }
            }
        },
        Operation::CreateComposite(local_index, absolute_path, values) => {
            let ty = if let Some(path) = absolute_path {
                crate::ast::ty::Type::new_path_resolved(crate::Span::new_null(), crate::ast::path::TypeBinding::Struct(path.clone()))
            }
            else {
                todo!("CreateComposite - tuple")
            };
            let ti = out_state.outer_state.type_info(&ty);
            if ti.is_primitive_like() {
                todo!("CreateComposite - {ty:?}")
            }
            else {
                let VariableValue::StackSlot(ss) = out_state.variables[local_index.0] else { todo!("CreateComposite to non SS"); };
                let f = ti.as_composite().unwrap();
                for (value, fld) in Iterator::zip(values.iter(), f.iter()) {
                    let value = out_state.read_value(value);
                    match value {
                    ReadValue::Empty => {},
                    ReadValue::Single(value) => { out_state.builder.ins().stack_store(value, ss, fld.ofs as i32); },
                    ReadValue::Stack(stack_slot, _, _) => todo!(),
                    }
                }
            }
        },
        Operation::CreateDataVariant(local_index, absolute_path, _, values) => todo!(),
        Operation::BinOp(local_index, value_r, bin_op, value_l) => {
            use ms_ir::BinOp;
            let x = out_state.read_value_single(value_l);
            let y = out_state.read_value_single(value_r);
            let v = match ir.locals[local_index.0].kind {
                TypeKind::Integer(_) => match bin_op {
                    BinOp::Add => out_state.builder.ins().iadd(x, y),
                    BinOp::Sub => out_state.builder.ins().isub(x, y),
                    BinOp::Mul => out_state.builder.ins().imul(x, y),
                    BinOp::Div => todo!(),
                    BinOp::Rem => todo!(),
                    BinOp::BitOr => todo!(),
                    BinOp::BitAnd => todo!(),
                    BinOp::BitXor => todo!(),
                },
                TypeKind::Pointer { is_const: _, ref inner } => match bin_op {
                    BinOp::Add => {
                        let ti = out_state.outer_state.type_info(&inner);
                        let y = out_state.builder.ins().imul_imm(y, ti.size() as i64);
                        out_state.builder.ins().iadd(x, y)
                    },
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
        Operation::Cast(local_index, value) => {
            let x = out_state.read_value_single(value);
            let dst = get_types(&ir.locals[local_index.0]);
            let src = out_state.value_type(value).map(|t| get_types(t));
            match (dst, src)
            {
            (TranslatedType::Empty, None) => todo!(),
            (TranslatedType::Empty, Some(_)) => todo!(),
            (TranslatedType::Single(_), None) => todo!(),
            (TranslatedType::Single(_), Some(_)) => todo!(),
            (TranslatedType::Complex, None) => todo!(),
            (TranslatedType::Complex, Some(_)) => todo!(),
            }
            todo!("cast")
        },
        Operation::UniOp(local_index, uni_op, value) => {
            use ms_ir::UniOp;
            let x = out_state.read_value_single(value);
            let v = match ir.locals[local_index.0].kind {
                TypeKind::Integer(_) => match uni_op {
                    UniOp::Not => todo!("UniOp - not int"),
                    UniOp::Neg => out_state.builder.ins().ineg(x),
                    },
                _ => todo!("UniOp on {}", ir.locals[local_index.0]),
            };
            out_state.store_value(local_index, v);
        },
        Operation::BitShift(local_index, value_l, bit_shift, value_r) => {
            let x = out_state.read_value_single(value_l);
            let y = out_state.read_value_single(value_r);
            let v = match ir.locals[local_index.0].kind {
                TypeKind::Integer(crate::ast::ty::IntClass::PtrDiff|crate::ast::ty::IntClass::Signed(_)) => match bit_shift
                    {
                    ms_ir::BitShift::Left => todo!(),
                    ms_ir::BitShift::Right => todo!(),
                    },
                TypeKind::Integer(crate::ast::ty::IntClass::PtrInt|crate::ast::ty::IntClass::Unsigned(_)) => match bit_shift
                    {
                    ms_ir::BitShift::Left => todo!(),
                    ms_ir::BitShift::Right => out_state.builder.ins().ushr(x, y),
                    },
                _ => todo!("BitShift on {}", ir.locals[local_index.0]),
                };
            out_state.store_value(local_index, v);
        },
        Operation::BorrowLocal(local_index, _, slot, wrapper_list) => {
            let VariableValue::StackSlot(ss) = out_state.variables[slot.0] else { panic!("BorrowLocal not of a slot - {:?}", out_state.variables[slot.0]); };
            let (dyn_ofs, ofs, ty) = out_state.get_offset_from_wrappers(&ir.locals[slot.0], wrapper_list);
            let ptr_val = out_state.builder.ins().stack_addr(out_state.ctxt.ptr_ty(), ss, ofs as i32);
            let ptr_val = match dyn_ofs {
                None => ptr_val,
                Some(dyn_ofs) => out_state.builder.ins().iadd(ptr_val, dyn_ofs),
            };
            out_state.store_value(local_index, ptr_val);
        },
        Operation::BorrowGlobal(dst, _, absolute_path, wrapper_list) => todo!(),
        Operation::PointerOffset(dst, _, other_ptr, wrappers) => {
            let TypeKind::Pointer { is_const: _, inner: ref ty } = out_state.ir.locals[other_ptr.0].kind else { panic!("PointerOffset on non-pointer") };
            let (dyn_ofs, ofs, _) = out_state.get_offset_from_wrappers(ty, wrappers);
            let value = out_state.read_value_single(&ms_ir::Value::Local(*other_ptr, Default::default()));
            let value = out_state.builder.ins().iadd_imm(value, ofs as i64);
            let value = match dyn_ofs {
                None => value,
                Some(dyn_ofs) => out_state.builder.ins().iadd(value, dyn_ofs),
            };
            out_state.store_value(dst, value);
        },
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
        let value = out_state.read_value(value);
        if let Some(return_ptr) = return_ptr {
            let flags = cr_ir::MemFlags::new()
                .with_checked()
                .with_aligned()
                .with_notrap()
                ;
            match value {
            ReadValue::Empty => todo!(),
            ReadValue::Single(value) => {
                out_state.builder.ins().store(flags, value, return_ptr, 0);
                },
            ReadValue::Stack(src_ss, src_ty, src_ofs) => {
                    let ti = out_state.outer_state.type_info(&src_ty);
                    // TODO: respect alignment (do byte loads if alignment is `<4`)
                    // TODO: only do the below if the type isn't a primtive (e.g. borrowed)
                    for i in 0 .. ti.size() / 4 {
                        let value = out_state.builder.ins().stack_load(cr_ir::types::I32, src_ss, (src_ofs + i * 4) as i32);
                        out_state.builder.ins().store(flags, value, return_ptr, (i * 4) as i32);
                    }
                    let ofs = ti.size() - ti.size() % 4;
                    for i in 0 .. ti.size() % 4 {
                        let value = out_state.builder.ins().stack_load(cr_ir::types::I8, src_ss, (src_ofs + ofs + i) as i32);
                        out_state.builder.ins().store(flags, value, return_ptr, (ofs + i) as i32);
                    }
                },
            }
        }
        else {
            let vals = value.into_iter(out_state).collect::<Vec<_>>();
            out_state.builder.ins().return_(&vals);
        }
    },
    Terminator::Compare { lhs, op, rhs, if_true, if_false } => {
        use ms_ir::CmpOp;
        use cr_ir::condcodes::IntCC;
        let ty = match (out_state.value_type(lhs), out_state.value_type(rhs))
            {
            (Some(t), _) => Some(t),
            (_, Some(t)) => Some(t),
            _ => None,
            };
        let is_signed = match ty.map(|v| &v.kind)
            {
            None => None,
            Some(TypeKind::Integer(ik)) => Some(match ik
                {
                crate::ast::ty::IntClass::Unsigned(_)
                | crate::ast::ty::IntClass::PtrInt => false,
                crate::ast::ty::IntClass::Signed(_)
                | crate::ast::ty::IntClass::PtrDiff => false,
                }),
            Some(TypeKind::Pointer { .. }) => Some(false),
            Some(_) => None,
            };
        let cnd = match (op,is_signed) {
            (CmpOp::Eq, _) => IntCC::Equal,
            (CmpOp::Ne, _) => IntCC::NotEqual,
            (CmpOp::Lt, Some(true )) => IntCC::SignedLessThan,
            (CmpOp::Lt, Some(false)) => IntCC::UnsignedLessThan,
            (CmpOp::Le, _) => todo!(),
            (CmpOp::Gt, _) => todo!(),
            (CmpOp::Ge, _) => todo!(),
            (CmpOp::Lt, None) => panic!("Comparison on non-integer-alike"),
        };
        let x = out_state.read_value_single(lhs);
        let y = out_state.read_value_single(rhs);
        let cnd = out_state.builder.ins().icmp(cnd, x, y);
        let (then_label, then_args) = out_state.get_jump(if_true);
        let (else_label, else_args) = out_state.get_jump(if_false);
        out_state.builder.ins().brif( cnd, then_label, &then_args, else_label, &else_args);
    },
    Terminator::MatchEnum { value, index, if_true, if_false  } => todo!("terminator: Match"),
    Terminator::CallPath { dst, tgt, path, args } => {
        let (fr,is_indirect) = out_state.get_function(path, args.len(), &|i| &args[i]);
        let args = out_state.get_val_list(&args);

        if is_indirect {
            let VariableValue::StackSlot(rv_ss) = out_state.variables[dst.0] else { panic!("Return value for indirect-return function call not a stack slot"); };
            let mut args = args;
            let addr = out_state.builder.ins().stack_addr(out_state.ctxt.ptr_ty(), rv_ss, 0);
            args.insert(0, addr);
            out_state.builder.ins().call(fr, &args);
        }
        else {
            let call_inst = out_state.builder.ins().call(fr, &args);

            let values = out_state.builder.inst_results(call_inst);
            // TODO: Large return values may end up being an implicit pointer
            match values {
            [] => {},
            [value] => out_state.store_value(dst, *value),
            [..] => todo!("Multiple returns?"),
            }
        }

        let (call_label, call_args) = out_state.get_jump(tgt);
        out_state.builder.ins().jump(call_label, &call_args);
    },
    Terminator::CallValue { dst, tgt, ptr, args } => todo!("terminator: CallValue"),
    }
}

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
    Stack(cr_ir::StackSlot, crate::ast::Type, usize),
    //Multiple,
}
impl ReadValue {
    fn into_iter(self, state: &mut State) -> impl Iterator<Item=cr_ir::Value> {
        match self {
        ReadValue::Empty => None,
        ReadValue::Stack(ss, ty, ofs) => {
            match get_types(&ty) {
            TranslatedType::Empty => None,
            TranslatedType::Single(ty) => Some( state.builder.ins().stack_load(ty, ss, ofs as i32) ),
            TranslatedType::Complex => todo!("into_iter for stack slot - non-trivial: {}", ty),
            }
        },
        ReadValue::Single(value) => Some(value),
        }.into_iter()
    }
}
struct State<'ir, 'a, 'a1> {
    ctxt: &'ir mut Context,
    outer_state: &'ir super::InnerState<'ir>,
    ir: &'ir super::ir::Expr,
    builder: &'a mut ::cranelift_frontend::FunctionBuilder<'a1>,
    blocks: Vec<cr_ir::Block>,
    variables: Vec<VariableValue>,
}
impl<'ir, 'a, 'a1> State<'ir, 'a, 'a1> {
    fn get_offset_from_wrappers<'ty>(&mut self, mut ty: &'ty crate::ast::Type, wrappers: &ms_ir::WrapperList) -> (Option<cr_ir::Value>, usize, &'ty crate::ast::Type)
    where
        'ir: 'ty
    {
        let mut ofs_dyn = None;
        let mut ofs_fixed = 0;
        for w in wrappers.iter() {
            use ms_ir::Wrapper;
            match w {
            Wrapper::Field(idx) => {
                match &ty.kind {
                TypeKind::Array { inner, count: _ } | TypeKind::UnsizedArray(inner) => {
                    let ti = self.outer_state.type_info(&inner);
                    let ofs = (idx * ti.size()) as usize;
                    ofs_fixed += ofs;
                    ty = inner;
                },
                TypeKind::Named(tp) => {
                    let ti = self.outer_state.type_info(&ty);
                    let crate::ast::ty::TypePath::Resolved(p) = tp else { panic!(); };
                    use crate::ast::path::TypeBinding;
                    match p {
                    TypeBinding::Alias(_) => panic!("Getting field on type alias? {}", ty),
                    TypeBinding::Union(absolute_path) => todo!(),
                    TypeBinding::Struct(absolute_path) => {
                        let f = &ti.as_composite().unwrap()[idx];
                        ofs_fixed += f.ofs;
                        ty = self.outer_state.field_types[absolute_path][idx];
                    },
                    TypeBinding::ValueEnum(_) => panic!("Getting field on value enum? {}", ty),
                    TypeBinding::DataEnum(absolute_path) => todo!(),
                    TypeBinding::EnumVariant(_, _) => panic!("Actual type bound to an EnumVariant (only valid for match)"),
                    }
                }
                TypeKind::Tuple(inner_tys) => {
                    let ti = self.outer_state.type_info(&ty);
                    let f = &ti.as_composite().unwrap()[idx];
                    ofs_fixed += f.ofs;
                    ty = &inner_tys[idx];
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
                ofs_dyn = Some(if let Some(ofs_val) = ofs_dyn {
                    self.builder.ins().iadd(ofs_val, ofs)
                }
                else {
                    ofs
                });
                ty = inner;
            },
            }
        }
        (ofs_dyn, ofs_fixed, ty)
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
            let val_ty = &self.ir.locals[local_index.0];
            let (ofs_dyn, ofs_fixed, ty) = self.get_offset_from_wrappers(val_ty, wrapper_list);
            if ofs_fixed > 0 || ofs_dyn.is_some() {
                let VariableValue::StackSlot(stack_slot) = self.variables[local_index.0] else {
                    todo!("Indirect access on non-slot: {local_index:?} : {:?}", self.variables[local_index.0]);
                };
                return match get_types(ty)
                {
                TranslatedType::Empty => ReadValue::Empty,
                TranslatedType::Complex => todo!("Field to complex type, will need to memcpy to destination"),
                TranslatedType::Single(cr_ty) => ReadValue::Single(match ofs_dyn
                    {
                    None => self.builder.ins().stack_load(cr_ty, stack_slot, ofs_fixed as i32),
                    Some(ofs_dyn) => {
                        let ptr = self.builder.ins().stack_addr(self.ctxt.ptr_ty(), stack_slot, ofs_fixed as i32);
                        let ptr = self.builder.ins().iadd(ptr, ofs_dyn);
                        let flags = cr_ir::MemFlags::new();
                        self.builder.ins().load(cr_ty, flags, ptr, 0)
                    }
                    })
                };
            }
            match self.variables[local_index.0] {
            VariableValue::Unassigned => panic!("Unassigned slot? _{}", local_index.0),
            VariableValue::Empty => ReadValue::Empty,
            VariableValue::StackSlot(stack_slot) => ReadValue::Stack(stack_slot, ty.clone(), 0),
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
            let (ofs_dyn, ofs_fixed, ty) = self.get_offset_from_wrappers(&val_ty, wrappers);
            let ptr = if let Some(ofs) = ofs_dyn { self.builder.ins().iadd(ptr, ofs) } else { ptr };
            let mut flags = cr_ir::MemFlags::new();
            if false {
                flags.set_notrap();
            }
            match get_types(ty)
            {
            TranslatedType::Empty => todo!("Deref to empty type"),
            TranslatedType::Complex => todo!("Deref to complex type, will need to memcpy to destination"),
            TranslatedType::Single(cr_ty) => ReadValue::Single(self.builder.ins().load(cr_ty, flags, ptr, ofs_fixed as i32)),
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
        Value::IntegerLiteral(value, int_cls) =>
            match u64::try_from(*value) {
            Ok(v) => ReadValue::Single(self.builder.ins().iconst(get_int_ty(int_cls), v as i64)),
            Err(_) => todo!("big integer literal"),
            },
        Value::FunctionPointer(absolute_path, function_pointer_ty) => todo!(),
        }
    }
    /// Read a trivial value (must fit into a register), helper for number ops
    fn read_value_single(&mut self, value: &ms_ir::Value) -> cr_ir::Value {
        match self.read_value(value)
        {
        ReadValue::Empty => panic!("Expected a register-sized value, but got a zero-sized value: {:?}", value),
        ReadValue::Single(value) => value,
        ReadValue::Stack(ss, ty, ofs) => {
            match get_types(&ty)
            {
            TranslatedType::Empty => panic!("Expected a register-sized value, but got a zero-sized value: {:?}: {}", value, ty),
            TranslatedType::Single(cr_ty) => self.builder.ins().stack_load(cr_ty, ss, ofs as i32),
            TranslatedType::Complex => todo!("Complex type from `read_value_single`? {}", ty),
            }
            }
        }
    }

    fn get_ty_from_wrappers<'out>(&'out self, mut ty: &'out crate::ast::Type, wrapper_list: &ms_ir::WrapperList) -> &'out crate::ast::Type
    {
        for w in wrapper_list.iter() {
            use ms_ir::Wrapper;
            match w {
            Wrapper::Field(idx) => {
                match &ty.kind {
                TypeKind::Array { inner, count: _ } | TypeKind::UnsizedArray(inner) => {
                    ty = inner;
                },
                TypeKind::Named(tp) => {
                    let crate::ast::ty::TypePath::Resolved(p) = tp else { panic!(); };
                    use crate::ast::path::TypeBinding;
                    match p {
                    TypeBinding::Alias(_) => panic!("Field on type alias"),
                    TypeBinding::Union(absolute_path) => todo!(),
                    TypeBinding::Struct(absolute_path) => {
                        ty = self.outer_state.field_types[absolute_path][idx];
                    },
                    TypeBinding::ValueEnum(_) => panic!("Field on value enum?"),
                    TypeBinding::DataEnum(absolute_path) => todo!(),
                    TypeBinding::EnumVariant(_, _) => panic!("Bound to enum variant?"),
                    }
                }
                TypeKind::Tuple(inner_tys) => {
                    ty = &inner_tys[idx];
                },
                _ => todo!("Get field offset for {ty} #{}", idx),
                }
            },
            Wrapper::IndexBySlot(_) => {
                let (TypeKind::Array { inner, count: _ } | TypeKind::UnsizedArray(inner)) = &ty.kind else {
                    panic!("Indexing on invalid type: {}", ty);
                };
                ty = inner;
                }
            }
        }
        ty
    }
    fn value_type(&self, value: &ms_ir::Value) -> Option<&crate::ast::Type> {
        match value {
        ms_ir::Value::Unreachable => None,
        ms_ir::Value::ImplicitUnit => todo!("Get type of unit - why is this being compared? (only place where `value_type` is called"),
        ms_ir::Value::Local(local_index, wrapper_list) => Some(self.get_ty_from_wrappers(&self.ir.locals[local_index.0], wrapper_list)),
        ms_ir::Value::Named(absolute_path, wrapper_list) => {
            let Some(ty) = self.outer_state.statics.get(absolute_path) else { panic!("Undefined static {}", absolute_path) };
            Some(self.get_ty_from_wrappers(ty, wrapper_list))
        },
        ms_ir::Value::Deref { ptr, wrappers } => {
            let TypeKind::Pointer { is_const: _, inner: ref val_ty } = self.ir.locals[ptr.0].kind else {
                panic!("Deref on non-pointer: {ptr:?} - {ty}", ty=self.ir.locals[ptr.0]);
            };
            Some(self.get_ty_from_wrappers(val_ty, wrappers))
        },
        ms_ir::Value::StringLiteral(_) => todo!("Get type of StringLiteral - why are you comparing with a string literal? (only place where `value_type` is called)"),
        ms_ir::Value::IntegerLiteral(_, int_cls) => {
            use crate::ast::ty::{Type,IntClass};
            const fn make_ty(ic: crate::ast::ty::IntClass) -> Type {
                Type { kind: crate::ast::ty::TypeKind::Integer(ic), span: crate::Span::new_null() }
            }
            match int_cls
            {
            IntClass::PtrInt  => { static T: Type = make_ty(IntClass::PtrInt); Some(&T) },
            IntClass::PtrDiff => { static T: Type = make_ty(IntClass::PtrDiff); Some(&T) },
            IntClass::Signed(0) => { static T: Type = make_ty(IntClass::Signed(0)); Some(&T) },
            IntClass::Signed(1) => { static T: Type = make_ty(IntClass::Signed(1)); Some(&T) },
            IntClass::Signed(2) => { static T: Type = make_ty(IntClass::Signed(2)); Some(&T) },
            IntClass::Signed(3) => { static T: Type = make_ty(IntClass::Signed(3)); Some(&T) },
            IntClass::Signed(_) => todo!(),
            IntClass::Unsigned(0) => { static T: Type = make_ty(IntClass::Unsigned(0)); Some(&T) },
            IntClass::Unsigned(1) => { static T: Type = make_ty(IntClass::Unsigned(1)); Some(&T) },
            IntClass::Unsigned(2) => { static T: Type = make_ty(IntClass::Unsigned(2)); Some(&T) },
            IntClass::Unsigned(3) => { static T: Type = make_ty(IntClass::Unsigned(3)); Some(&T) },
            IntClass::Unsigned(_) => todo!(),
            }
            },
        ms_ir::Value::FunctionPointer(_absolute_path, _) => None,//todo!(),
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
                .flat_map(|l| self.read_value(&ms_ir::Value::Local(*l, Default::default())).into_iter(self))
                .collect::<Vec<_>>()
        )
    }
    fn get_val_list(&mut self, values: &[ms_ir::Value]) -> Vec<cr_ir::Value> {
        values.iter()
            .flat_map(|l| self.read_value(l).into_iter(self))
            .collect::<Vec<_>>()
    }

    fn get_function(&mut self, path: &crate::ast::path::AbsolutePath, arg_count: usize, get_arg: &dyn Fn(usize)->&'ir ms_ir::Value) -> (cr_ir::FuncRef,bool,) {
        let Some((name, DeclaredValueItem::Function { sig, variadic_after, indirect_return })) = self.ctxt.functions.get(path) else { panic!("{} not defined as a function" ,path) };
        let mut sig = sig;
        let mut tmp_sig;
        // Handle variadic functions
        if let Some(n) = *variadic_after {
            tmp_sig = sig.clone();
            for i in n .. arg_count {
                let ty = self.value_type(get_arg(i)).unwrap();
                self.ctxt.to_abi_params(self.outer_state, &mut tmp_sig.params, ty);
            }
            sig = &tmp_sig;
        }
        // TODO: Cache
        let name = self.builder.func.declare_imported_user_function(name.get_user().unwrap().clone());
        // TODO: Cache
        let signature = self.builder.import_signature(sig.clone());
        let rv = self.builder.import_function(cr_ir::ExtFuncData {
            name: cr_ir::ExternalName::User(name),
            signature,
            colocated: false
        });
        (rv, *indirect_return)
    }
    fn get_global(&mut self, path: &crate::ast::path::AbsolutePath) -> (&crate::ast::Type, cr_ir::GlobalValue) {
        let Some(ty) = self.outer_state.statics.get(path) else { panic!("Undefined static {}", path) };
        let Some((name, DeclaredValueItem::ExternStatic)) = self.ctxt.functions.get(path) else { panic!("{} not defined as a static",path) };
        let name = self.builder.func.declare_imported_user_function(name.get_user().unwrap().clone());
        let gv_base = self.builder.create_global_value(cr_ir::GlobalValueData::Symbol {
            name: cr_ir::ExternalName::User(name),
            offset: 0.into(),
            colocated: false,
            tls: false
        });
        let gv = self.builder.create_global_value(cr_ir::GlobalValueData::Load {
            base: gv_base,
            offset: 0.into(),
            flags: cr_ir::MemFlags::new(),
            global_type: match get_types(ty)
                {
                TranslatedType::Empty => cr_ir::types::INVALID,
                TranslatedType::Complex => todo!("Global to complex type, will need to memcpy to destination"),
                TranslatedType::Single(cr_ty) => cr_ty,
                },
        });
        (ty,gv)
    }
}

enum TranslatedType {
    Empty,
    Single(cr_ir::Type),
    Complex,
}
fn get_int_ty(int_class: &crate::ast::ty::IntClass) -> cr_ir::Type {
    use cr_ir::types as t;
    use crate::ast::ty::IntClass;
    match int_class
    {
    IntClass::PtrInt|IntClass::PtrDiff => t::I64,
    IntClass::Signed(shift)|IntClass::Unsigned(shift) => match shift
        {
        0 => t::I8,
        1 => t::I16,
        2 => t::I32,
        3 => t::I64,
        4 => t::I128,
        _ => panic!("Too-large integer type"),
        },
    }
}
fn get_types(ty: &crate::ast::Type) -> TranslatedType {
    use cr_ir::types as t;
    let ptr = t::I64;
    match &ty.kind {
    TypeKind::Infer { .. } | TypeKind::TypeOf(..) => panic!("Unexpanded {:?}", ty),
    TypeKind::Void | TypeKind::UnsizedArray(..) => panic!("Unexpected {:?}", ty),

    TypeKind::Bool => TranslatedType::Single(t::I8),
    TypeKind::Integer(int_class) => TranslatedType::Single(get_int_ty(int_class)),
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