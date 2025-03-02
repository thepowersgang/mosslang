use super::Result;
use super::Lexer;
use super::lex;

use crate::ast::{Pattern, PatternTy};

pub fn parse_pattern(lex: &mut Lexer) -> Result<crate::ast::Pattern> {
    parse_pattern_inner(lex, Vec::new())
}
fn parse_pattern_inner(lex: &mut Lexer, mut bindings: Vec<crate::Ident>) -> Result<crate::ast::Pattern> {
    let span = lex.cur_span();
    Ok(if let Some(p) = super::opt_parse_path(lex)? {
        if p.is_trivial() {
            if lex.opt_consume_punct(lex::Punct::At)? {
                // Binding!
                let mut bindings = bindings;
                bindings.push(p.into_trivial().ok().unwrap());
                // Recurse
                return parse_pattern_inner(lex, bindings);
            }
        }

        if lex.opt_consume_punct(lex::Punct::ParenOpen)? {
            todo!("parse_pattern - Tuple struct");
        }
        else if lex.opt_consume_punct(lex::Punct::BraceOpen)? {
            todo!("parse_pattern - named struct");
        }
        else {
            match p.into_trivial()
            {
            Ok(i) => Pattern { span, bindings, ty: PatternTy::MaybeBind(i) },
            Err(p) => Pattern { span, bindings, ty: PatternTy::NamedValue(p) },
            }
        }
    }
    else if lex.opt_consume_punct(lex::Punct::ParenOpen)? {
        let mut pats = Vec::new();
        loop {
            if lex.opt_consume_punct(lex::Punct::ParenClose)? {
                break;
            }
            pats.push(parse_pattern_inner(lex, Vec::new())?);
            if !lex.opt_consume_punct(lex::Punct::Comma)? {
                lex.consume_punct(lex::Punct::ParenClose)?;
                break;
            }
        }
        Pattern {
            span,
            bindings,
            ty: PatternTy::Tuple(pats),
        }
    }
    else if lex.opt_consume_rword(lex::ReservedWord::Mut)? {
        bindings.push(lex.consume_ident()?);
        if lex.opt_consume_punct(lex::Punct::At)? {
            return parse_pattern_inner(lex, bindings);
        }
        else {
            Pattern { span, bindings, ty: PatternTy::Any }
        }
    }
    else {
        todo!("parse_pattern - {:?}", lex.peek());
    })
}