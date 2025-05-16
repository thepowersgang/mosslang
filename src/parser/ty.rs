use super::Result;
use super::Lexer;
use super::lex;
use crate::ast::Type;

pub fn parse_type(lex: &mut Lexer) -> Result<crate::ast::Type> {
    let ps = lex.start_span();
    if let Some(p) = super::opt_parse_path(lex)? {
        if p.is_trivial() {
            use crate::ast::ty::IntClass;
            match p.components[0].to_string().as_str() {
            "_" => return Ok(Type::new_infer()),
            "u8"  => return Ok(Type::new_integer(IntClass::Unsigned(0))),
            "u16" => return Ok(Type::new_integer(IntClass::Unsigned(1))),
            "u32" => return Ok(Type::new_integer(IntClass::Unsigned(2))),
            "u64" => return Ok(Type::new_integer(IntClass::Unsigned(3))),
            "i8"  => return Ok(Type::new_integer(IntClass::Signed(0))),
            "i16" => return Ok(Type::new_integer(IntClass::Signed(1))),
            "i32" => return Ok(Type::new_integer(IntClass::Signed(2))),
            "i64" => return Ok(Type::new_integer(IntClass::Signed(3))),

            "usize" => return Ok(Type::new_integer(IntClass::PtrInt)),
            "isize" => return Ok(Type::new_integer(IntClass::PtrDiff)),
            "void" => return Ok(Type::new_void()),
            "bool" => return Ok(Type::new_bool()),
            _ => {},
            }
        }
        Ok( Type::new_path(p) )
    }
    else {
        match lex.consume_no_eof()?
        {
        // `(` - Tuple or grouping
        lex::Token::Punct(lex::Punct::ParenOpen) => {
            if lex.opt_consume_punct(lex::Punct::ParenClose)? {
                Ok( Type::new_unit() )
            }
            else {
                let inner = parse_type(lex)?;
                if lex.opt_consume_punct(lex::Punct::ParenClose)? {
                    Ok( inner )
                }
                else {
                    let mut v = vec![inner];
                    while lex.opt_consume_punct(lex::Punct::Comma)? {
                        if lex.check_punct(lex::Punct::ParenClose)? {
                            break ;
                        }
                        v.push(parse_type(lex)?);
                    }
                    lex.consume_punct(lex::Punct::ParenClose)?;
                    Ok( Type::new_tuple(v) )
                }
            }
            },
        // '[' - Array or slice
        lex::Token::Punct(lex::Punct::SquareOpen) => {
            let inner = parse_type(lex)?;
            lex.consume_punct(lex::Punct::Semicolon)?;
            //if lex.opt_consume_punct(lex::Punct::Semicolon)? {
                let count = super::expr::parse_root_expr(lex)?;
                lex.consume_punct(lex::Punct::SquareClose)?;
                Ok(match count.e.kind
                    {
                    crate::ast::expr::ExprKind::LiteralInteger(v, _) if v < usize::MAX as u128 => Type::new_array_fixed(inner, v as usize),
                    _ => Type::new_array(inner, count),
                    })
            //}
            //else {
            //    lex.consume_punct(lex::Punct::SquareClose)?;
            //    Ok( Type::new_slice(inner) )
            //}
            },
        // '*' ['const'|'mut'] - Pointer
        lex::Token::Punct(lex::Punct::Star) => {
            let is_const = if lex.opt_consume_rword(lex::ReservedWord::Const)? {
                    true
                }
                else if lex.opt_consume_rword(lex::ReservedWord::Mut)? {
                    false
                }
                else {
                    return Err(lex.unexpected());
                };
            let inner = parse_type(lex)?;
            Ok( Type::new_ptr(is_const, inner) )
            },
        lex::Token::RWord(lex::ReservedWord::Typeof) => {
            let e = super::parse_root_expr(lex)?;
            Ok( Type::new_typeof(e) )
            },
        t => todo!("{}: parse_type - {:?}", ps, t),
        }
    }
}