
use crate::ast::items::{Item,ItemType};

use super::Result;
use super::lex::{self,Lexer};
use super::{parse_attributes,parse_pattern,parse_type,parse_root_expr,parse_root_block};

/// Top-level of a file/module parse
pub fn parse_module(lex: &mut Lexer, mod_attrs: &mut Vec<crate::ast::Attribute>) -> Result<crate::ast::items::Module> {
    let mut rv: crate::ast::items::Module = Default::default();
    loop {
        // If the current token is a close-brace, then return without consuming it
        // - Parent will consume
        if let Some(lex::Token::Punct(lex::Punct::BraceClose)) = lex.peek() {
            break
        }

        // Parse module attributes
        let mut attributes = parse_attributes(lex, rv.items.is_empty().then_some(mod_attrs))?;

        // TODO: Get publicity? (no publicity implemented)

        match match lex.consume() {
            None => {
                if !attributes.is_empty() {
                    // TODO: Error, attributes with no item.
                }
                break
                },
            Some(v) => v,
            }
        {
        lex::Token::RWord(lex::ReservedWord::Type) => {
            println!("type alias");
            let name = lex.consume_ident()?;
            lex.consume_punct(lex::Punct::Equals)?;
            let ty = parse_type(lex)?;
            lex.consume_punct(lex::Punct::Semicolon)?;
            rv.items.push(crate::ast::items::Item {
                name: Some(name),
                ty: crate::ast::items::ItemType::TypeAlias(ty),
                attributes,
                });
        },
        lex::Token::RWord(lex::ReservedWord::Enum) => {
            let (name, i) = parse_enum(lex, &mut attributes)?;
            rv.items.push(crate::ast::items::Item {
                name: Some(name),
                ty: crate::ast::items::ItemType::Enum(i),
                attributes,
                });
        },
        lex::Token::RWord(lex::ReservedWord::Struct) => {
            let (name, i) = parse_struct(lex, &mut attributes)?;
            rv.items.push(crate::ast::items::Item {
                name: Some(name),
                ty: crate::ast::items::ItemType::Struct(i),
                attributes,
                });
        },

        // `extern` [ "abi" ] (`fn`|`{`)
        lex::Token::RWord(lex::ReservedWord::Extern) => {
            let abi = lex.opt_consume_string()?;
            match lex.consume_no_eof()?
            {
            // Extern block
            lex::Token::Punct(lex::Punct::BraceOpen) => {
                println!("extern block");
                rv.items.push(crate::ast::items::Item {
                    name: None,
                    ty: crate::ast::items::ItemType::ExternBlock(parse_extern_block(lex, abi, &mut attributes)?),
                    attributes,
                });
                },
            lex::Token::RWord(lex::ReservedWord::Fn) => {
                println!("extern fn");
                let (name, i) = parse_fn(lex, &mut attributes, abi)?;
                rv.items.push(crate::ast::items::Item {
                    name: Some(name),
                    ty: crate::ast::items::ItemType::Function(i),
                    attributes,
                });
                },
            t => todo!("{}: other extern? {:?}", lex.start_span(), t),
            }
        },

        // `static NAME: Type = value;`
        lex::Token::RWord(lex::ReservedWord::Static) => {
            let name = lex.consume_ident()?;
            lex.consume_punct(lex::Punct::Colon)?;
            let ty = parse_type(lex)?;
            lex.consume_punct(lex::Punct::Equals)?;
            let val = parse_root_expr(lex)?;
            lex.consume_punct(lex::Punct::Semicolon)?;

            rv.items.push(Item {
                attributes,
                name: Some(name),
                ty: ItemType::Static(crate::ast::items::Static {
                    ty,
                    value: val,
                    }),
            });
            },

        // A constant value
        // `const NAME: Type = value;`
        lex::Token::RWord(lex::ReservedWord::Const) => {
            let name = lex.consume_ident()?;
            lex.consume_punct(lex::Punct::Colon)?;
            let ty = parse_type(lex)?;
            lex.consume_punct(lex::Punct::Equals)?;
            let val = parse_root_expr(lex)?;
            lex.consume_punct(lex::Punct::Semicolon)?;

            rv.items.push(Item {
                attributes,
                name: Some(name),
                ty: ItemType::Constant(crate::ast::items::Constant {
                    ty,
                    value: val,
                    }),
            });
            },

        // `fn function(args: Types) -> RetTy { code }`
        lex::Token::RWord(lex::ReservedWord::Fn) => {
            let (name, i) = parse_fn(lex, &mut attributes, None)?;
            rv.items.push(crate::ast::items::Item {
                name: Some(name),
                ty: crate::ast::items::ItemType::Function(i),
                attributes,
            });
            },
        t => todo!("{}: parse error: expected reserved word, got {:?}", lex.start_span(), t),
        }
    }
    Ok(rv)
}

fn parse_enum(lex: &mut Lexer, outer_attrs: &mut Vec<crate::ast::Attribute>) -> Result<(crate::Ident,crate::ast::items::Enum)> {
    let name = lex.consume_ident()?;
    let mut variants = Vec::new();
    let mut is_incomplete = false;
    lex.consume_punct(lex::Punct::BraceOpen)?;
    loop {
        if lex.opt_consume_punct(lex::Punct::TripleDot)? {
            is_incomplete = true;
            break;
        }
        if let Some(lex::Token::Punct(lex::Punct::BraceClose)) = lex.peek() {
            break;
        }
        
        let attributes = parse_attributes(lex, variants.is_empty().then_some(outer_attrs))?;
        let name = lex.consume_ident()?;

        let ty = if lex.opt_consume_punct(lex::Punct::ParenOpen)? {
                todo!("data enums");
            }
            else if lex.opt_consume_punct(lex::Punct::Equals)? {
                crate::ast::items::EnumVariantTy::Value( parse_root_expr(lex)? )
            }
            else {
                crate::ast::items::EnumVariantTy::Bare
            };

        variants.push(crate::ast::items::EnumVariant {
            attributes,
            name,
            ty,
        });

        if !lex.opt_consume_punct(lex::Punct::Comma)? {
            break;
        }
    }
    lex.consume_punct(lex::Punct::BraceClose)?;
    Ok((name, crate::ast::items::Enum {
        variants,
        is_incomplete,
    }))
}

fn parse_struct(lex: &mut Lexer, outer_attrs: &mut Vec<crate::ast::Attribute>) -> Result<(crate::Ident,crate::ast::items::Struct)> {
    let name = lex.consume_ident()?;
    let mut fields = Vec::new();
    let mut is_incomplete = false;
    lex.consume_punct(lex::Punct::BraceOpen)?;
    loop {
        if lex.opt_consume_punct(lex::Punct::TripleDot)? {
            is_incomplete = true;
            break;
        }
        if let Some(lex::Token::Punct(lex::Punct::BraceClose)) = lex.peek() {
            break;
        }
        
        let attributes = parse_attributes(lex, fields.is_empty().then_some(outer_attrs))?;
        let name = lex.consume_ident()?;
        lex.consume_punct(lex::Punct::Colon)?;
        let ty = parse_type(lex)?;

        fields.push(crate::ast::items::StructField {
            attributes,
            name,
            ty,
        });

        if !lex.opt_consume_punct(lex::Punct::Comma)? {
            break;
        }
    }
    lex.consume_punct(lex::Punct::BraceClose)?;
    Ok((name, crate::ast::items::Struct {
        fields,
        is_incomplete,
    }))
}

/// Extern block, after the `{` has been consumed
fn parse_extern_block(lex: &mut Lexer, abi: crate::ast::AbiSpec, mut outer_attrs: &mut Vec<crate::ast::Attribute>) -> Result<crate::ast::items::ExternBlock> {
    let mut items = Vec::new();
    loop {
        let attributes = parse_attributes( lex, items.is_empty().then_some(&mut outer_attrs) )?;
        match lex.consume_no_eof()?
        {
        lex::Token::Punct(lex::Punct::BraceClose) => break,
        lex::Token::RWord(lex::ReservedWord::Fn) => {
            let (name, sig) = parse_fn_hdr(lex, None)?;
            lex.consume_punct(lex::Punct::Semicolon)?;
            items.push(crate::ast::items::ExternItem {
                name,
                attributes,
                ty: crate::ast::items::ExternItemType::Function(sig),
                });
            },
        lex::Token::RWord(lex::ReservedWord::Static) => {
            let name = lex.consume_ident()?;
            lex.consume_punct(lex::Punct::Colon)?;
            let ty = parse_type(lex)?;
            lex.consume_punct(lex::Punct::Semicolon)?;
            items.push(crate::ast::items::ExternItem {
                name,
                attributes,
                ty: crate::ast::items::ExternItemType::Static(crate::ast::items::ExternStatic { ty }),
                });
            },
        t => todo!("{}: extern block: unexpected token {:?}", lex.start_span(), t),
        }
    }
    Ok(crate::ast::items::ExternBlock {
        abi,
        items,
    })
}

/// Parse a function definition, with optional ABI specification
fn parse_fn(lex: &mut Lexer, attributes: &mut Vec<crate::ast::Attribute>, abi: crate::ast::AbiSpec) -> Result<(crate::Ident, crate::ast::items::Function)> {
    let _ = attributes; // TODO: Pass attribute list to `parse_root_block`
    let (name, sig) = parse_fn_hdr(lex, abi)?;
    let code = parse_root_block(lex)?;
    Ok((name, crate::ast::items::Function {
        sig,
        code,
        }))
}

/// Parse a function signature (header)
fn parse_fn_hdr(lex: &mut Lexer, abi: crate::ast::AbiSpec) -> Result<(crate::Ident,crate::ast::items::FunctionSignature)> {

    let name = lex.consume_ident()?;
    lex.consume_punct(lex::Punct::ParenOpen)?;

    // Parse argument list
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

    // Return type, defaulting to "no data"
    let ret_ty = if lex.opt_consume_punct(lex::Punct::ThinArrow)? {
            parse_type(lex)?
        }
        else {
            crate::ast::Type::new_unit()
        };
    Ok((name, crate::ast::items::FunctionSignature { abi, args, is_variadic, ret: ret_ty }))
}