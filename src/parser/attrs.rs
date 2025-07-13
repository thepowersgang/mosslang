// cspell:ignore Punct
use super::{lex,Lexer,Result};

/// Parse a sequence of attributes
pub fn parse_attributes(lex: &mut Lexer, mut outer_attributes: Option<&mut Vec<crate::ast::Attribute>>) -> Result<Vec<crate::ast::Attribute>> {
    let mut attributes = Vec::new();
    while lex.opt_consume_punct(lex::Punct::Hash)? {
        if lex.opt_consume_punct(lex::Punct::Bang)? {
            match (&mut outer_attributes, attributes.is_empty())
            {
            // If outer list is available, and the inner hasn't been populated - allow outer attrs
            (Some(outer_attributes), true) => {
                outer_attributes.push(parse_attr(lex)?);
                }
            _ => {
                // TODO: Error, inner attr after items
                panic!("{}: Outer attributes seen", "?")
                }
            }
        }
        else {
            attributes.push(parse_attr(lex)?);
        }
    }
    Ok( attributes ) 
}

/// Parse an attribute BEFORE the opening square bracket has been consumed
fn parse_attr(lex: &mut Lexer) -> Result<crate::ast::Attribute> {
    lex.consume_punct(lex::Punct::SquareOpen)?;
    // Expect an ident
    let i = lex.consume_ident()?;
    let d = if lex.opt_consume_punct(lex::Punct::Equals)? {
            // Argument string
            let s = lex.consume_string()?;
            crate::ast::AttributeData::Value(s)
        }
        else if lex.opt_consume_punct(lex::Punct::ParenOpen)? {
            let mut items = Vec::new();
            while ! lex.opt_consume_punct(lex::Punct::ParenClose)? {
                items.push( parse_attr(lex)? );
                if !lex.opt_consume_punct(lex::Punct::Comma)? {
                    lex.consume_punct(lex::Punct::ParenClose)?;
                    break ;
                }
            }
            crate::ast::AttributeData::SubItems(items)
        }
        else {
            // No data
            crate::ast::AttributeData::None
        };
    lex.consume_punct(lex::Punct::SquareClose)?;
    Ok(crate::ast::Attribute {
        name: i,
        data: d,
    })
}
