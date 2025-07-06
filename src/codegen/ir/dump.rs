
use super::{Terminator,Operation,Value};


pub fn dump_fcn(out: &mut dyn ::std::io::Write, name: &crate::Ident, sig: &crate::ast::items::FunctionSignature, ir: &super::Expr)
{
    write!(out, "fn {name}( ").unwrap();
    for (i,(_, ty)) in sig.args.iter().enumerate() {
        write!(out, "_{}: {}, ", i, ty).unwrap();
    }
    write!(out, ") {{\n").unwrap();
    dump(&mut IndentFile(&mut *out, true), &ir).unwrap();
    write!(out, "}}\n\n").unwrap();
}

pub fn dump_static(out: &mut dyn ::std::io::Write, name: &crate::Ident, ty: &crate::ast::Type, ir: &super::Expr)
{
    write!(out, "static {name}: {ty} = {{\n").unwrap();
    dump(&mut IndentFile(&mut *out, true), &ir).unwrap();
    write!(out, "}};\n\n").unwrap();
}

pub fn dump(out: &mut dyn ::std::io::Write, src: &super::Expr) -> ::std::io::Result<()>
{
    for (i,var_ty) in src.locals.iter().enumerate() {
        writeln!(out, "let _{}: {};", i, var_ty)?;
    }
    for (i,block) in src.blocks.iter().enumerate() {
        write!(out, "bb{i}(")?;
        for a in &block.args {
            write!(out, "{},", F(a))?;
        }
        writeln!(out, "): {{")?;
        for stmt in &block.statements {
            write!(out, "    ")?;
            match stmt {
            Operation::Alloca { dst, ty }
                => write!(out, "{} = alloca {}", F(dst), ty)?,
            Operation::AssignLocal(dst, value)
                => write!(out, "{} = {}", F(dst), F(value))?,
            Operation::AssignDeref(ptr, value)
                => write!(out, "*{} = {}", F(ptr), F(value))?,
            Operation::CreateComposite(dst, absolute_path, values) => {
                if let Some(absolute_path) = absolute_path {
                    write!(out, "{} = {{ ", F(dst))?;
                    for a in values {
                        write!(out, "{}, ", F(a))?;
                    }
                    write!(out, "}}: {}", absolute_path)?;
                }
                else {
                    write!(out, "{} = ( ", F(dst))?;
                    for a in values {
                        write!(out, "{}, ", F(a))?;
                    }
                    write!(out, ")")?;
                }
            },
            Operation::CreateDataVariant(dst, absolute_path, idx, values) => {
                write!(out, "{} = {}#{} ( ", F(dst), absolute_path, idx)?;
                for a in values {
                    write!(out, "{}, ", F(a))?;
                }
                write!(out, ")")?;
            },
            Operation::Cast(dst, value)
                => write!(out, "{} = {} as _", F(dst), F(value))?,
            Operation::BinOp(dst, value, bin_op, value1)
                => write!(out, "{} = {} {} {}", F(dst), F(value), F(bin_op), F(value1))?,
            Operation::UniOp(dst, uni_op, value)
                => write!(out, "{} = {} {}", F(dst), F(uni_op), F(value))?,
            Operation::BitShift(dst, value, bit_shift, value1)
                => write!(out, "{} = {} {} {}", F(dst), F(value), F(bit_shift), F(value1))?,
            Operation::BorrowLocal(dst, is_mut, slot, wrapper_list)
                => write!(out, "{} = &{} {}{}", F(dst), m(is_mut), F(slot), F(wrapper_list))?,
            Operation::BorrowGlobal(dst, is_mut, path, wrapper_list)
                => write!(out, "{} = &{} {}{}", F(dst), m(is_mut), path, F(wrapper_list))?,
            Operation::PointerOffset(dst, is_mut, local_index1, wrapper_list)
                => write!(out, "{} = &{} (*{}){}", F(dst), m(is_mut), F(local_index1), F(wrapper_list))?,
            }
            writeln!(out, ";")?;

            fn m(is_mut: &bool) -> &'static str {
                if *is_mut { "mut" } else { "" }
            }
        }
        write!(out, "}} ")?;
        match &block.terminator {
        Terminator::Goto(tgt) => write!(out, "goto {}", F(tgt))?,
        Terminator::Return(value) => write!(out, "return {}", F(value))?,
        Terminator::Compare { lhs, op, rhs, if_true, if_false }
            => write!(out, "if {} {} {} {{ goto {} }} else {{ goto {} }}", F(lhs), F(op), F(rhs), F(if_true), F(if_false))?,
        Terminator::MatchEnum { value, index, if_true, if_false }
            => write!(out, "if {} is #{} {{ goto {} }} else {{ goto {} }}", F(value), index, F(if_true), F(if_false))?,
        Terminator::CallPath { dst, path, args, tgt } => {
            write!(out, "{} = {}( ", F(dst), path)?;
            for a in args {
                write!(out, "{}, ", F(a))?;
            }
            write!(out, ") goto {}", F(tgt))?;
        },
        Terminator::CallValue{ dst: local_dst, ptr, args, tgt } => {
            write!(out, "{} = ({})( ", F(local_dst), F(ptr))?;
            for a in args {
                write!(out, "{}, ", F(a))?;
            }
            write!(out, ") goto {}", F(tgt))?;
        },
        Terminator::Unreachable => write!(out, " !")?,
        }
        writeln!(out, ";")?;
    }
    Ok(())
}

/// Helper: Applies pretty formatting to various types
struct F<'a, T>(&'a T);
impl<'a> ::std::fmt::Display for F<'a, super::LocalIndex> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "_{}", self.0 .0)
    }
}
impl<'a> ::std::fmt::Display for F<'a, super::WrapperList> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for w in self.0.iter() {
            match w {
            super::Wrapper::Field(idx) => write!(f, ".{}", idx)?,
            super::Wrapper::IndexBySlot(local_index) => write!(f, "[{}]", F(&local_index))?,
            }
        }
        Ok( () )
    }
}
impl<'a> ::std::fmt::Display for F<'a, super::Value> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
        Value::Unreachable => write!(f, "!"),
        Value::ImplicitUnit => write!(f, "()"),
        Value::Local(local_index, wrapper_list) => write!(f, "{}{}", F(local_index), F(wrapper_list)),
        Value::Named(absolute_path, wrapper_list) => write!(f, "{}{}", absolute_path, F(wrapper_list)),
        Value::Deref { ptr, wrappers } => write!(f, "(*{}){}", F(ptr), F(wrappers)),
        Value::StringLiteral(string_literal) => write!(f, "{:?}", string_literal),
        Value::IntegerLiteral(v, ty) => write!(f, "{:#x} /*{:?}*/", v, ty),
        Value::FunctionPointer(path, super::FunctionPointerTy::Function) => write!(f, "{}", path),
        Value::FunctionPointer(path, super::FunctionPointerTy::Struct) => write!(f, "{}", path),
        Value::FunctionPointer(path, super::FunctionPointerTy::DataEnum(idx)) => write!(f, "{}#{}", path, idx),
        }
    }
}
impl<'a> ::std::fmt::Display for F<'a, super::BlockIndex> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.0 .0)
    }
}
impl<'a> ::std::fmt::Display for F<'a, super::JumpTarget> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.0.index)?;
        if !self.0.args.is_empty() {
            f.write_str("(")?;
            for a in &self.0.args {
                write!(f, "{},", F(a))?;
            }
            f.write_str(")")?;
        }
        Ok(())
    }
}

impl<'a> ::std::fmt::Display for F<'a, super::CmpOp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self.0
            {
            super::CmpOp::Eq => "==",
            super::CmpOp::Ne => "!=",
            super::CmpOp::Lt => "< ",
            super::CmpOp::Le => ">=",
            super::CmpOp::Gt => "> ",
            super::CmpOp::Ge => ">=",
            })
    }
}

impl<'a> ::std::fmt::Display for F<'a, super::UniOp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self.0
            {
            super::UniOp::Not => "!",
            super::UniOp::Neg => "-",
            })
    }
}
impl<'a> ::std::fmt::Display for F<'a, super::BinOp> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self.0
            {
            super::BinOp::Add => "+",
            super::BinOp::Sub => "-",
            super::BinOp::Mul => "*",
            super::BinOp::Div => "/",
            super::BinOp::Rem => "%",
            super::BinOp::BitOr  => "|",
            super::BinOp::BitAnd => "&",
            super::BinOp::BitXor => "^",
            })
    }
}
impl<'a> ::std::fmt::Display for F<'a, super::BitShift> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self.0
            {
            super::BitShift::Left => "<<",
            super::BitShift::Right => ">>",
            })
    }
}

struct IndentFile<F>(F, bool);
impl<F> IndentFile<F>
where
    F: ::std::io::Write
{
    fn write_seg(&mut self, rv: &mut usize, buf: &[u8]) -> Option<std::io::Result<usize>> {
        if ::std::mem::replace(&mut self.1, false) {
            match self.0.write(b"    ") {
            Err(e) => return Some(Err(e)),
            Ok(_) => {},
            }
        }
        match self.0.write(buf) {
        Err(e) => return Some(Err(e)),
        Ok(v) => {
            *rv += v;
            if v != buf.len() {
                return Some(Ok(v))
            }
        }
        }
        None
    }
}
impl<F> ::std::io::Write for IndentFile<F>
where
    F: ::std::io::Write
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut it = buf.split(|&v| v == b'\n');
        let mut cur = it.next().unwrap();

        let mut rv = 0;

        for v in it {
            if let Some(v) = self.write_seg(&mut rv, cur) {
                return v;
            }
            if let Some(v) = self.write_seg(&mut rv, b"\n") {
                return v;
            }
            // This is only reached if the buffer contained at least one newline
            self.1 = true;
            cur = v;
        }

        if cur.len() == 0 {
            self.1 = true;
        }
        else {
            if let Some(v) = self.write_seg(&mut rv, cur) {
                return v;
            }
        }
        assert!(rv <= buf.len());
        Ok(rv)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}