use super::Result;
use super::Lexer;
use super::lex;

pub fn parse_type(lex: &mut Lexer) -> Result<crate::ast::Type> {
    if let Some(p) = super::opt_parse_path(lex)? {
        Ok( crate::ast::Type::new_path(p) )
    }
    else {
        match lex.consume_no_eof()?
        {
        // `(` - Tuple or grouping
        lex::Token::Punct(lex::Punct::ParenOpen) => {
            if lex.opt_consume_punct(lex::Punct::ParenClose)? {
                Ok( crate::ast::Type::new_unit() )
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
                    Ok( crate::ast::Type::new_tuple(v) )
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
                Ok( crate::ast::Type::new_array(inner, count) )
            //}
            //else {
            //    lex.consume_punct(lex::Punct::SquareClose)?;
            //    Ok( crate::ast::Type::new_slice(inner) )
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
            Ok( crate::ast::Type::new_ptr(is_const, inner) )
            },
        t => todo!("parse_type - {:?}", t),
        }
    }
}