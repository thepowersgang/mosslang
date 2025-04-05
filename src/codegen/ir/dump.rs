
use super::{Terminator,Operation,Value};

pub fn dump(dst: &mut dyn ::std::io::Write, src: &super::Expr) -> ::std::io::Result<()>
{
    for (i,var_ty) in src.locals.iter().enumerate() {
        writeln!(dst, "let _{}: {}", i, var_ty)?;
    }
    for (i,block) in src.blocks.iter().enumerate() {
        writeln!(dst, "bb{}: {{", i)?;
        for stmt in &block.statements {
            write!(dst, "    ")?;
            match stmt {
            Operation::AssignLocal(local_index, wrapper_list, value)
                => write!(dst, "{}{} = {}", F(local_index), F(wrapper_list), F(value))?,
            Operation::AssignNamed(absolute_path, value)
                => write!(dst, "{} = {}", absolute_path, F(value))?,
            Operation::CreateComposite(local_index, absolute_path, values) => {
                if let Some(absolute_path) = absolute_path {
                    write!(dst, "{} = {{ ", F(local_index))?;
                    for a in values {
                        write!(dst, "{}, ", F(a))?;
                    }
                    write!(dst, "}}: {}", absolute_path)?;
                }
                else {
                    write!(dst, "{} = ( ", F(local_index))?;
                    for a in values {
                        write!(dst, "{}, ", F(a))?;
                    }
                    write!(dst, ")")?;
                }
            },
            Operation::BinOp(local_index, value, bin_op, value1)
                => write!(dst, "{} = {} {} {}", F(local_index), F(value), F(bin_op), F(value1))?,
            Operation::UniOp(local_index, uni_op, value)
                => write!(dst, "{} = {} {}", F(local_index), F(uni_op), F(value))?,
            Operation::BitShift(local_index, value, bit_shift, value1)
                => write!(dst, "{} = {} {} {}", F(local_index), F(value), F(bit_shift), F(value1))?,
            Operation::BorrowLocal(local_index, _, local_index1, wrapper_list)
                => write!(dst, "{} = & {}{}", F(local_index), F(local_index1), F(wrapper_list))?,
            Operation::PointerOffset(local_index, _, local_index1, wrapper_list)
                => write!(dst, "{} = & (*{}){}", F(local_index), F(local_index1), F(wrapper_list))?,
            }
            writeln!(dst, ";")?;
        }
        write!(dst, "}} ")?;
        match &block.terminator {
        Terminator::Goto(block_index) => write!(dst, "goto {}", F(block_index))?,
        Terminator::Return(value) => write!(dst, "return {}", F(value))?,
        Terminator::Compare(value, cmp_op, value1, block_index, block_index1)
            => write!(dst, "if {} {} {} {{ goto {} }} else {{ goto {} }}", F(value), F(cmp_op), F(value1), F(block_index), F(block_index1))?,
        Terminator::CallPath(local_index, block_index, absolute_path, values) => {
            write!(dst, "{} = {}( ", F(local_index), absolute_path)?;
            for a in values {
                write!(dst, "{}, ", F(a))?;
            }
            write!(dst, ") goto {}", F(block_index))?;
        },
        Terminator::Unreachable => write!(dst, " !")?,
        }
        writeln!(dst, ";")?;
    }
    Ok(())
}

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
        Value::Deref { ptr, wrappers } => write!(f, "*{}{}", F(ptr), F(wrappers)),
        Value::StringLiteral(string_literal) => write!(f, "{:?}", string_literal),
        Value::IntegerLiteral(v) => write!(f, "{:#x}", v),
        }
    }
}
impl<'a> ::std::fmt::Display for F<'a, super::BlockIndex> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.0 .0)
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