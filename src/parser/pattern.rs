// cspell:ignore Punct rword
use super::Result;
use super::Lexer;
use super::lex;

use crate::ast::Pattern;
use crate::ast::pattern::{PatternBinding,PatternTy, Value as PatternValue, NamedValue};

pub fn parse_pattern(lex: &mut Lexer) -> Result<crate::ast::Pattern> {
    let ps = lex.start_span();
    let v = parse_pattern_inner(lex, Vec::new())?;
    if lex.opt_consume_punct(lex::Punct::Pipe)? {
        let mut sub_pats = Vec::new();
        sub_pats.push(v);
        loop {
            sub_pats.push( parse_pattern_inner(lex, Vec::new())? );
            if !lex.opt_consume_punct(lex::Punct::Pipe)? {
                break;
            }
        }
        Ok(make_pat(lex.end_span(&ps), Vec::new(), PatternTy::Multiple(sub_pats)))
    }
    else {
        Ok(v)
    }
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
            match lex.peek() {
            Some(lex::Token::Punct(lex::Punct::DoubleDot|lex::Punct::TripleDot)) => {
                parse_pattern_after_value(lex, &ps, bindings, PatternValue::NamedValue(NamedValue::Unbound(p)))?
            }
            _ => {
                make_pat(lex.end_span(&ps), bindings, match p.into_trivial()
                    {
                    Ok(i) => PatternTy::MaybeBind(i),
                    Err(p) => PatternTy::ValueSingle(PatternValue::NamedValue(NamedValue::Unbound(p))),
                    })
            }
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
        match lex.peek() {
        Some(lex::Token::Literal(..)) => {
            let v1 = parse_pattern_value(lex, &ps)?;
            parse_pattern_after_value(lex, &ps, bindings, v1)?
            },
        t @ _ => todo!("{}: parse_pattern - {:?}", ps, t),
        }
    })
}

fn parse_pattern_value(lex: &mut Lexer, ps: &lex::ProtoSpan) -> Result<crate::ast::pattern::Value> {
    Ok(match lex.consume_no_eof()? {
    lex::Token::Literal(lex::Literal::Integer(val, _cls)) => {
        crate::ast::pattern::Value::Integer(val/*, cls*/)
        }
    t @ _ => todo!("{}: parse_pattern_value - {:?}", ps, t),
    })
}
fn parse_pattern_after_value(lex: &mut Lexer, ps: &lex::ProtoSpan, bindings: Vec<PatternBinding>, v1: crate::ast::pattern::Value) -> Result<crate::ast::Pattern> {
    let p = if lex.opt_consume_punct(lex::Punct::DoubleDot)? {
            let v2 = parse_pattern_value(lex, ps)?;
            PatternTy::ValueRangeExcl(v1, v2)
        }
        else if lex.opt_consume_punct(lex::Punct::TripleDot)? /*|| lex.opt_consume_punct(lex::Punct::DoubleDotEq)?*/ {
            let v2 = parse_pattern_value(lex, ps)?;
            PatternTy::ValueRangeIncl(v1, v2)
        }
        else {
            PatternTy::ValueSingle(v1)
        };
    Ok(make_pat(lex.end_span(&ps), bindings, p))
}