
use crate::ast::items::{self,Item,ItemType};

mod lex;
mod expr;
mod pattern;
mod ty;

use self::pattern::parse_pattern;
use self::ty::parse_type;

use lex::Lexer;

#[derive(Default)]
pub struct ParsedModuleFile {
    self_attrs: Vec<crate::ast::Attribute>,
    module: crate::ast::items::Module,
}

type Result<T> = ::std::result::Result<T, Error>;
#[derive(Debug)]
pub enum Error {
    Io(::std::io::Error),
}
impl From<::std::io::Error> for Error {
    fn from(v: ::std::io::Error) -> Self { Error::Io(v) }
}

pub fn parse_file(input: &::std::path::Path) -> Result<ParsedModuleFile>
{
    let mut lex = lex::Lexer::from_path(input)?;
    let mut top_attrs = Vec::new();
    let rv = ParsedModuleFile {
        module: parse_module(&mut lex, &mut top_attrs)?,
        self_attrs: top_attrs,
        };
    if lex.peek().is_some() {
        todo!("Error: Tray characters")
    }
    Ok(rv)
}

fn parse_module(lex: &mut Lexer, mod_attrs: &mut Vec<crate::ast::Attribute>) -> Result<crate::ast::items::Module> {
    let mut rv: crate::ast::items::Module = Default::default();
    loop {
        if let Some(lex::Token::Punct(lex::Punct::BraceClose)) = lex.peek() {
            break
        }

        let mut attributes = Vec::new();
        while lex.opt_consume_punct(lex::Punct::Hash)? {
            if lex.opt_consume_punct(lex::Punct::Bang)? {

                if !attributes.is_empty() || !rv.items.is_empty() {
                    // TODO: Error, inner attr after items
                }

                mod_attrs.push(parse_attr(lex)?);
            }
            else {
                attributes.push(parse_attr(lex)?);
            }
        }

        // Get publicity? (no publicity implemented)

        match match lex.consume() {
            None => {
                if !attributes.is_empty() {
                    // TODO: Error
                }
                break
                },
            Some(v) => v,
            }
        {
        lex::Token::RWord(lex::ReservedWord::Struct) => todo!("struct"),
        lex::Token::RWord(lex::ReservedWord::Extern) => {
            let abi = if let lex::Token::Literal(lex::Literal::String(_)) = lex.peek_no_eof()? {
                    match lex.consume_no_eof()?
                    {
                    lex::Token::Literal(lex::Literal::String(s)) => Some(s),
                    _ => unreachable!(),
                    }
                } else {
                    None
                };
            match lex.consume_no_eof()?
            {
            lex::Token::Punct(lex::Punct::BraceOpen) => {
                let mut items = Vec::new();
                loop {
                    let mut attributes = Vec::new();
                    while lex.opt_consume_punct(lex::Punct::Hash)? {
                        if lex.opt_consume_punct(lex::Punct::Bang)? {
            
                            if !attributes.is_empty() || !items.is_empty() {
                                // TODO: Error, inner attr after items
                            }
            
                            mod_attrs.push(parse_attr(lex)?);
                        }
                        else {
                            attributes.push(parse_attr(lex)?);
                        }
                    }
                    match lex.consume_no_eof()?
                    {
                    lex::Token::Punct(lex::Punct::BraceClose) => break,
                    lex::Token::RWord(lex::ReservedWord::Fn) => {
                        let (name, sig) = parse_fn_hdr(lex)?;
                        lex.consume_punct(lex::Punct::Semicolon)?;
                        items.push(crate::ast::items::ExternItem {
                            name,
                            attributes,
                            ty: crate::ast::items::ExternItemType::Function(sig),
                            });
                        },
                    _ => todo!("extern block: unexpected"),
                    }
                }
                
                println!("extern block");
                rv.items.push(crate::ast::items::Item {
                    attributes,
                    name: None,
                    ty: crate::ast::items::ItemType::ExternBlock(crate::ast::items::ExternBlock {
                        items,
                        }),
                });
                },
            lex::Token::RWord(lex::ReservedWord::Fn) => {
                rv.items.push(parse_fn(lex, attributes, abi)?);
                },
            _ => todo!("other extern?"),
            }
        },

        // `static NAME: Type = value;`
        lex::Token::RWord(lex::ReservedWord::Static) => {
            let name = lex.consume_ident()?;
            lex.consume_punct(lex::Punct::Colon)?;
            let ty = parse_type(lex)?;
            lex.consume_punct(lex::Punct::Equals)?;
            let val = expr::parse_root_expr(lex)?;
            lex.consume_punct(lex::Punct::Semicolon)?;

            rv.items.push(Item {
                attributes,
                name: Some(name),
                ty: ItemType::Static(items::Static {
                    ty,
                    value: val,
                    }),
            });
            },

        // `fn function(args: Types) -> RetTy { code }`
        lex::Token::RWord(lex::ReservedWord::Fn) => {
            rv.items.push(parse_fn(lex, attributes, None)?);
            },
        t => todo!("parse error: expected reserved word, got {:?}", t),
        }
    }
    Ok(rv)
}

fn parse_attr(lex: &mut Lexer) -> Result<crate::ast::Attribute> {
    todo!("parse_attr")
}

fn parse_fn_hdr(lex: &mut Lexer) -> Result<(crate::Ident,crate::ast::items::FunctionSignature)> {

    let name = lex.consume_ident()?;
    lex.consume_punct(lex::Punct::ParenOpen)?;
    let mut args = Vec::new();
    let is_variadic = loop {
        if lex.check_punct(lex::Punct::ParenClose)? {
            break false;
        }
        if lex.opt_consume_punct(lex::Punct::TripleDot)? {
            break true;
        }
        let pat = parse_pattern(lex)?;
        lex.consume_punct(lex::Punct::Colon)?;
        let ty = parse_type(lex)?;

        args.push( (pat, ty) );
        if !lex.opt_consume_punct(lex::Punct::Comma)? {
            break false;
        }
    };
    lex.consume_punct(lex::Punct::ParenClose)?;

    let ret_ty = if lex.opt_consume_punct(lex::Punct::ThinArrow)? {
            parse_type(lex)?
        }
        else {
            crate::ast::Type::new_unit()
        };
    Ok((name, crate::ast::items::FunctionSignature { args, is_variadic, ret: ret_ty }))
}

fn parse_fn(lex: &mut Lexer, attributes: Vec<crate::ast::Attribute>, abi: Option<Vec<u8>>) -> Result<crate::ast::items::Item> {
    let (name, sig) = parse_fn_hdr(lex)?;
    let code = expr::parse_root_block(lex)?;
    Ok( Item {
        attributes,
        name: Some(name),
        ty: ItemType::Function(crate::ast::items::Function {
            sig,
            code,
            })
        } )
}

fn opt_parse_path(lex: &mut Lexer) -> Result<Option<crate::ast::Path>> {
    Ok(match lex.peek_no_eof()? {
    lex::Token::Ident(_)
    | lex::Token::Punct(lex::Punct::DoubleColon)
    | lex::Token::RWord(lex::ReservedWord::Self_)
    | lex::Token::RWord(lex::ReservedWord::Super)
    => Some(parse_path(lex)?),
    _ => None,
    })
}
fn parse_path(lex: &mut Lexer) -> Result<crate::ast::Path> {
    let root = if lex.opt_consume_punct(lex::Punct::DoubleColon)? {
            crate::ast::path::Root::Root
        }
        else if lex.opt_consume_rword(lex::ReservedWord::Self_)? {
            lex.check_punct(lex::Punct::DoubleColon)?;
            crate::ast::path::Root::Current
        }
        else if lex.opt_consume_rword(lex::ReservedWord::Super)? {
            lex.check_punct(lex::Punct::DoubleColon)?;
            let mut extra_count = 0;
            while lex.opt_consume_rword(lex::ReservedWord::Super)? {
                lex.check_punct(lex::Punct::DoubleColon)?;
                extra_count += 1;
            }
            crate::ast::path::Root::Super(extra_count)
        }
        else {
            crate::ast::path::Root::None
        };
    let mut components = Vec::new();
    loop {
        components.push(lex.consume_ident()?);
        if !lex.opt_consume_punct(lex::Punct::DoubleColon)? {
            break;
        }
    }
    Ok( crate::ast::Path { root, components })
}
