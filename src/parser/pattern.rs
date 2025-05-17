use super::Result;
use super::Lexer;
use super::lex;
use crate::ast::PatternBinding;

use crate::ast::{Pattern, PatternTy};

pub fn parse_pattern(lex: &mut Lexer) -> Result<crate::ast::Pattern> {
    parse_pattern_inner(lex, Vec::new())
}
fn make_pat(span: crate::Span, bindings: Vec<PatternBinding>, ty: PatternTy) -> Pattern {
    Pattern {
        data_ty: crate::ast::Type::new_infer(span.clone()),
        span,
        bindings,
        ty
        }
}
fn parse_pattern_inner(lex: &mut Lexer, mut bindings: Vec<PatternBinding>) -> Result<crate::ast::Pattern> {
    let ps = lex.start_span();
    Ok(if let Some(p) = super::opt_parse_path(lex)? {
        if p.is_trivial() {
            if lex.opt_consume_punct(lex::Punct::At)? {
                // Binding!
                let mut bindings = bindings;
                bindings.push(PatternBinding { name: p.into_trivial().ok().unwrap(), index: None });
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
            make_pat(lex.end_span(&ps), bindings, match p.into_trivial()
                {
                Ok(i) => PatternTy::MaybeBind(i),
                Err(p) => PatternTy::NamedValue(p, None),
                })
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
                if pats.len() == 1 {
                    return Ok(pats.into_iter().next().unwrap());
                }
                break;
            }
        }
        make_pat(lex.end_span(&ps), bindings, PatternTy::Tuple(pats))
    }
    else if lex.opt_consume_rword(lex::ReservedWord::Mut)? {
        bindings.push(PatternBinding { name: lex.consume_ident()?, index: None });
        if lex.opt_consume_punct(lex::Punct::At)? {
            return parse_pattern_inner(lex, bindings);
        }
        else {
            make_pat(lex.end_span(&ps), bindings, PatternTy::Any)
        }
    }
    // TODO: Integer patterns (with ranges, and `|`)
    else {
        //match lex.peek_no_eof()? {
        //lex::Token::Literal(lex::Literal::Integer(, ))
        //}
        todo!("{}: parse_pattern - {:?}", ps, lex.peek());
    })
}