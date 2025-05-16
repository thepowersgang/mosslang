use super::Result;
use super::Lexer;
use super::lex;
use crate::ast::PatternBinding;

use crate::ast::{Pattern, PatternTy};

pub fn parse_pattern(lex: &mut Lexer) -> Result<crate::ast::Pattern> {
    parse_pattern_inner(lex, Vec::new())
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
            match p.into_trivial()
            {
            Ok(i) => Pattern { span: lex.end_span(&ps), bindings, ty: PatternTy::MaybeBind(i), data_ty: crate::ast::Type::new_infer() },
            Err(p) => Pattern { span: lex.end_span(&ps), bindings, ty: PatternTy::NamedValue(p, None), data_ty: crate::ast::Type::new_infer()  },
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
                if pats.len() == 1 {
                    return Ok(pats.into_iter().next().unwrap());
                }
                break;
            }
        }
        Pattern {
            span: lex.end_span(&ps),
            bindings,
            ty: PatternTy::Tuple(pats),
            data_ty: crate::ast::Type::new_infer(),
        }
    }
    else if lex.opt_consume_rword(lex::ReservedWord::Mut)? {
        bindings.push(PatternBinding { name: lex.consume_ident()?, index: None });
        if lex.opt_consume_punct(lex::Punct::At)? {
            return parse_pattern_inner(lex, bindings);
        }
        else {
            Pattern { span: lex.end_span(&ps), bindings, ty: PatternTy::Any, data_ty: crate::ast::Type::new_infer()  }
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