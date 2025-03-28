
use super::{lex,Lexer,Result};

/// Helper: Optionally parse a path
/// 
/// Used by type parser
pub fn opt_parse_path(lex: &mut Lexer) -> Result<Option<crate::ast::Path>> {
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