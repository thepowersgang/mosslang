// cspell:ignore Punct rword parse_expr_nostruct
use super::lex::{Token, ReservedWord, Punct};
use crate::ast::expr as e;
use crate::INDENT;

/// Parse the root of an expression - expecting a block
pub fn parse_root_block(lex: &mut super::Lexer) -> super::Result<crate::ast::ExprRoot> {
    let ps = lex.start_span();
    lex.consume_punct(Punct::BraceOpen)?;
    let b = parse_block(lex)?;
    Ok(crate::ast::ExprRoot {
        e: e::ExprKind::Block(b).to_expr(lex.end_span(&ps)),
        variable_count: 0,
        variables: Default::default(),
    })
}
/// Parse the root of an expression, allowing any expression
pub fn parse_root_expr(lex: &mut super::Lexer) -> super::Result<crate::ast::ExprRoot> {
    let e = parse_expr(lex)?;
    Ok(crate::ast::ExprRoot {
        e,
        variable_count: 0,
        variables: Default::default(),
    })
}

fn parse_block(lex: &mut super::Lexer) -> super::Result<e::Block> {
    let _i = INDENT.inc("parse_block");
    let mut stmts = Vec::new();
    let end = loop {
        println!("{}parse_block: start {:?}", INDENT, lex.peek());
        if lex.check_punct(Punct::BraceClose)? {
            break None;
        }

        // A `let` statement
        if lex.opt_consume_rword(ReservedWord::Let)? {
            let pat = super::parse_pattern(lex)?;
            let ty = if lex.opt_consume_punct(Punct::Colon)? {
                    super::parse_type(lex)?
                }
                else {
                    crate::ast::Type::new_infer( lex.end_span(&lex.start_span()) )
                };
            let val = if lex.opt_consume_punct(Punct::Equals)? {
                Some(parse_expr(lex)?)
            } else {
                None
            };
            lex.consume_punct(Punct::Semicolon)?;
            stmts.push(e::Statement::Let(pat, ty, val));
            continue;
        }

        // Handle blocks slightly differently (they don't need trailing semicolons)
        match lex.peek_no_eof()?
        {
        Token::Punct(Punct::BraceOpen)
        | Token::RWord(ReservedWord::While)
        | Token::RWord(ReservedWord::If)
        | Token::RWord(ReservedWord::For)
        | Token::RWord(ReservedWord::Loop)
        //| Token::RWord(ReservedWord::Match)
        => {
            let e = parse_expr(lex)?;
            // If the next token is a closing brace - then this block-expr is the result of the current block
            if lex.check_punct(Punct::BraceClose)? {
                break Some(e);
            }

            // Ignore any trailing semicolon.
            if lex.opt_consume_punct(Punct::Semicolon)? {
            }
            stmts.push(e::Statement::Expr(e));
            },
        // 
        _ => {
            let e = parse_expr(lex)?;
            if lex.opt_consume_punct(Punct::Semicolon)? {
                stmts.push(e::Statement::Expr(e));
            }
            else {
                // This better be the end.
                //println!("parse_block: trailing {:?}", lex.peek());
                break Some(e);
            }
            }
        }
    };
    println!("{}parse_block: done {:?}", INDENT, lex.peek());
    lex.consume_punct(Punct::BraceClose)?;
    Ok(e::Block {
        statements: stmts,
        result: end.map(Box::new),
    })
}

fn parse_expr(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    let _i = INDENT.inc("parse_expr");
    let f = lex.set_flag(super::lex::Flag::NoStructLiteral, false);
    let rv = parse_expr_assign(lex);
    lex.set_flag(super::lex::Flag::NoStructLiteral, f);
    rv
}
/// Parse an expression, but don't allow struct literals at the top-level
fn parse_expr_nostruct(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    let _i = INDENT.inc("parse_expr_nostruct");
    let f = lex.set_flag(super::lex::Flag::NoStructLiteral, true);
    let rv = parse_expr_assign(lex);
    lex.set_flag(super::lex::Flag::NoStructLiteral, f);
    rv
}

// Precedence
macro_rules! def_left_assoc {
    ($name:ident, $next:ident, $v1:ident, $v2:ident => { $( $p:ident => $e:expr ),* }) => {
        fn $name(lex: &mut super::Lexer) -> super::Result<e::Expr> {
            let ps = lex.start_span();
            let mut $v1 = $next(lex)?;
            loop {
                match lex.peek_no_eof()?
                {
                $( Token::Punct(Punct::$p) => {
                    lex.consume();
                    let $v2 = $next(lex)?;
                    $v1 = $e.to_expr(lex.end_span(&ps));
                    }),*
                _ => break,
                }
            }
            Ok($v1)
        }

    }
}
macro_rules! def_bin_ops {
    ( $($name:ident { $($p:ident => $op:ident),* });* ; -> $next:ident  ) => {
        def_bin_ops!{ @outer $($name )* $next; $({ $($p => $op),* })* }
    };
    ( @outer $next:ident ; ) => {};
    ( @outer $name:ident $next:ident $($others:ident)*; $blk:tt $($other_blocks:tt)* ) => {
        def_bin_ops!{ @inner $name,$next $blk }
        def_bin_ops!{ @outer $next $($others)*; $($other_blocks)* }
    };
    ( @inner $name:ident,$next:ident { $($p:ident => $op:ident),* } ) => {
        def_left_assoc!{$name, $next, v1,v2 => {
            $( $p  => e::ExprKind::BinOp(e::BinOpTy::$op, Box::new(v1), Box::new(v2)) ),*
        }}
    };
}
// - Assignment
fn parse_expr_assign(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    use crate::ast::expr::AssignOp;
    let ps = lex.start_span();
    let lhs = parse_bin_ops_root(lex)?;
    fn op_equals(lex: &mut super::Lexer, ps: super::lex::ProtoSpan, lhs: e::Expr, op: Option<AssignOp>) -> super::Result<e::Expr> {
        lex.consume();
        Ok(e::ExprKind::Assign {
            slot: Box::new(lhs),
            op,
            value: Box::new(parse_bin_ops_root(lex)?),
        }.to_expr(lex.end_span(&ps)))
    }
    match lex.peek_no_eof()? {
    Token::Punct(Punct::Equals) => op_equals(lex, ps, lhs, None),
    Token::Punct(Punct::PlusEquals ) => op_equals(lex, ps, lhs, Some(AssignOp::Add)),
    Token::Punct(Punct::MinusEquals) => op_equals(lex, ps, lhs, Some(AssignOp::Sub)),
    Token::Punct(Punct::SlashEquals) => op_equals(lex, ps, lhs, Some(AssignOp::Div)),
    Token::Punct(Punct::StarEquals ) => op_equals(lex, ps, lhs, Some(AssignOp::Mul)),
    Token::Punct(Punct::PercentEquals) => op_equals(lex, ps, lhs, Some(AssignOp::Rem)),
    Token::Punct(Punct::PipeEquals)  => op_equals(lex, ps, lhs, Some(AssignOp::BitOr )),
    Token::Punct(Punct::AmpEquals)   => op_equals(lex, ps, lhs, Some(AssignOp::BitAnd)),
    Token::Punct(Punct::CaretEquals) => op_equals(lex, ps, lhs, Some(AssignOp::BitXor)),
    Token::Punct(Punct::DoubleLtEquals) => op_equals(lex, ps, lhs, Some(AssignOp::Shl)),
    Token::Punct(Punct::DoubleGtEquals) => op_equals(lex, ps, lhs, Some(AssignOp::Shr)),
    _ => Ok(lhs),
    }
}
fn parse_bin_ops_root(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    parse_expr_bool_or(lex)
}
// - Ranges
def_bin_ops!{
    // - Boolean OR, Boolean AND
    parse_expr_bool_or { DoublePipe => BoolOr, DoubleAmp => BoolAnd };
    // - Equalities
    //parse_expr_eq { };
    // - Orderings
    parse_expr_cmp {
        DoubleEqual => Equals,
        ExlamEqual => NotEquals,    // spell:disable-line
        Lt => Lt,
        Gt => Gt,
        LtEqual => LtEquals,
        GtEqual => GtEquals
    };
    // - Bitwise (OR, XOR, AND)
    parse_expr_bitor  { Pipe  => BitOr  };
    parse_expr_bitxor { Caret => BitXor };
    parse_expr_bitand { Amp   => BitAnd };
    // - Shifts
    parse_expr_shift { DoubleLt => Shl, DoubleGt => Shr };
    // - Add / Subtract
    parse_expr_add { Plus => Add, Minus => Sub };
    // - Mul / Div / Rem
    parse_expr_mul { Star => Mul, Slash => Div, Percent => Rem };
    -> parse_expr_cast
}
// - Cast (can't use the BinOp code, as `as` is reserved word not a punctuation... and the rhs is a type)
fn parse_expr_cast(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    let ps: super::lex::ProtoSpan = lex.start_span();
    let mut v = parse_expr_leading(lex)?;
    loop {
        if lex.opt_consume_rword(ReservedWord::As)? {
            let ty = super::parse_type(lex)?;
            v = e::ExprKind::Cast(Box::new(v), ty).to_expr(lex.end_span(&ps));
        }
        else {
            break;
        }
    }
    Ok(v)
}
/// Unary operators that come before the value
/// 
/// Leading deref, negate, and invert
fn parse_expr_leading(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    let ps = lex.start_span();
    Ok(if lex.opt_consume_punct(Punct::Star)? {
        e::ExprKind::Deref(Box::new(parse_expr_leading(lex)?)).to_expr(lex.end_span(&ps))
    }
    else if lex.opt_consume_punct(Punct::Amp)? {
        e::ExprKind::Addr(lex.opt_consume_rword(ReservedWord::Mut)?, Box::new(parse_expr_leading(lex)?)).to_expr(lex.end_span(&ps))
    }
    else if lex.opt_consume_punct(Punct::Bang)? {
        e::ExprKind::UniOp(e::UniOpTy::Invert, Box::new(parse_expr_leading(lex)?)).to_expr(lex.end_span(&ps))
    }
    else if lex.opt_consume_punct(Punct::Minus)? {
        e::ExprKind::UniOp(e::UniOpTy::Negate, Box::new(parse_expr_leading(lex)?)).to_expr(lex.end_span(&ps))
    }
    else {
        parse_expr_trailing(lex)?
    })
}
/// Trailing unary operations (call, index, field)
fn parse_expr_trailing(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    //let _i = INDENT.inc("parse_expr_trailing");
    let ps = lex.start_span();
    let mut v = parse_expr_value(lex)?;
    loop {
        // Function call (value) - `(...)(...)`
        if lex.opt_consume_punct(Punct::ParenOpen)? {
            let mut args = Vec::new();
            while ! lex.opt_consume_punct(Punct::ParenClose)? {

                args.push( parse_expr(lex)? );

                if ! lex.opt_consume_punct(Punct::Comma)? {
                    lex.consume_punct(Punct::ParenClose)?;
                    break;
                }
            }
            v = e::ExprKind::CallValue(Box::new(v), args).to_expr(lex.end_span(&ps));
        }
        // indexing - `(...)[...]`
        else if lex.opt_consume_punct(Punct::SquareOpen)? {
            let i = parse_expr(lex)?;
            lex.consume_punct(Punct::SquareClose)?;
            v = e::ExprKind::Index(Box::new(v), Box::new(i)).to_expr(lex.end_span(&ps));
        }
        // Field access/trailing-deref
        else if lex.opt_consume_punct(Punct::Dot)? {
            // Trailing deref - `(...).*`
            if lex.opt_consume_punct(Punct::Star)? {
                // Trailing deref
                v = e::ExprKind::Deref(Box::new(v)).to_expr(lex.end_span(&ps));
            }
            // Named field - `(...).fieldname`
            else if let Some(i) = lex.opt_consume_ident()? {
                v = e::ExprKind::FieldNamed(Box::new(v), i).to_expr(lex.end_span(&ps));
            }
            // Unnamed field - `(...).123`
            else if let &Token::Literal(super::lex::Literal::Integer(i, None)) = lex.peek_no_eof()? {
                lex.consume();
                v = e::ExprKind::FieldIndex(Box::new(v), i as usize).to_expr(lex.end_span(&ps));
            }
            else {
                todo!("parse_expr_trailing - unknown");
            }
        }
        else {
            break;
        }
    }
    println!("{}parse_expr_trailing: {:?}", INDENT, lex.peek());
    Ok(v)
}

fn parse_expr_opt(lex: &mut super::Lexer) -> super::Result<Option<e::Expr>> {
    Ok(match lex.peek_no_eof()? {
        Token::Punct(Punct::Semicolon)
        | Token::Punct(Punct::ParenClose)
        | Token::Punct(Punct::BraceClose)
        | Token::Punct(Punct::SquareClose)
        => None,
        _ => Some(parse_expr(lex)?),
        })
}

/// Bottom level values
fn parse_expr_value(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    let ps = lex.start_span();
    if let Some(p) = super::opt_parse_path(lex)? {
        return Ok(if !lex.has_flag(super::lex::Flag::NoStructLiteral) && lex.opt_consume_punct(Punct::BraceOpen)? {
            let mut values = Vec::new();
            while ! lex.opt_consume_punct(Punct::BraceClose)? {

                if lex.opt_consume_punct(Punct::DoubleDot)? {
                    todo!("Struct template/default")
                }

                let name = lex.consume_ident()?;
                lex.consume_punct(Punct::Colon)?;
                let val = parse_expr(lex)?;
                values.push((name, val));

                if ! lex.opt_consume_punct(Punct::Comma)? {
                    lex.consume_punct(Punct::BraceClose)?;
                    break;
                }
            }

            e::ExprKind::Struct(p, None, values).to_expr(lex.end_span(&ps))
        }
        else if lex.opt_consume_punct(Punct::ParenOpen)? {
            let mut args = Vec::new();
            while ! lex.opt_consume_punct(Punct::ParenClose)? {

                args.push( parse_expr(lex)? );

                if ! lex.opt_consume_punct(Punct::Comma)? {
                    lex.consume_punct(Punct::ParenClose)?;
                    break;
                }
            }
            e::ExprKind::CallPath(p, None, args).to_expr(lex.end_span(&ps))
        }
        else if lex.opt_consume_punct(Punct::Bang)? {
            todo!("{}: Rust macro?", lex.end_span(&ps));
        }
        else {
            e::ExprKind::NamedValue(p, None).to_expr(lex.end_span(&ps))
        })
    }
    Ok(match lex.peek_no_eof()?
    {
    Token::Literal(_) => {
        let Token::Literal(lit) = lex.consume().unwrap() else { unreachable!() };
        use crate::ast::ty::IntClass;
        match lit
        {
        super::lex::Literal::String(v) => e::ExprKind::LiteralString(v),
        super::lex::Literal::Integer(v, cls) => e::ExprKind::LiteralInteger(v, match cls {
            None => e::IntLitClass::Unspecified,
            Some(super::lex::IntClass::Pointer) => e::IntLitClass::Pointer,
            Some(super::lex::IntClass::I8 ) => e::IntLitClass::Integer(IntClass::Signed(0)),
            Some(super::lex::IntClass::I16) => e::IntLitClass::Integer(IntClass::Signed(1)),
            Some(super::lex::IntClass::I32) => e::IntLitClass::Integer(IntClass::Signed(2)),
            Some(super::lex::IntClass::I64) => e::IntLitClass::Integer(IntClass::Signed(4)),
            Some(super::lex::IntClass::U8 ) => e::IntLitClass::Integer(IntClass::Unsigned(0)),
            Some(super::lex::IntClass::U16) => e::IntLitClass::Integer(IntClass::Unsigned(1)),
            Some(super::lex::IntClass::U32) => e::IntLitClass::Integer(IntClass::Unsigned(2)),
            Some(super::lex::IntClass::U64) => e::IntLitClass::Integer(IntClass::Unsigned(4)),
        }),
        super::lex::Literal::Bool(v) => e::ExprKind::LiteralBoolean(v),
        super::lex::Literal::Float(v, cls) => todo!("Float literal: {:?} {:?}", v, cls),
        }.to_expr(lex.end_span(&ps))
        },
    Token::RWord(ReservedWord::Sizeof) => {
        lex.consume();
        lex.consume_punct(Punct::ParenOpen)?;
        let ty = super::ty::parse_type(lex)?;
        lex.consume_punct(Punct::ParenClose)?;
        e::ExprKind::TypeInfoSizeOf(ty).to_expr(lex.end_span(&ps))
    },
    Token::Punct(Punct::ParenOpen) => {
        lex.consume();
        let rv = parse_expr(lex)?;
        if lex.opt_consume_punct(Punct::Comma)? {
            // Tuple
            let mut items = vec![rv];
            loop {
                if lex.check_punct(Punct::ParenClose)? {
                    break
                }
                items.push(parse_expr(lex)?);
                if !lex.opt_consume_punct(Punct::Comma)? {
                    break
                }
            }
            lex.consume_punct(Punct::ParenClose)?;
            e::ExprKind::Tuple(items).to_expr(lex.end_span(&ps))
        }
        else {
            lex.consume_punct(Punct::ParenClose)?;
            rv
        }
        },
    Token::Punct(Punct::BraceOpen) => {
        lex.consume();
        e::ExprKind::Block(parse_block(lex)?).to_expr(lex.end_span(&ps))
        },
    Token::RWord(ReservedWord::Return) => {
        lex.consume();
        let v = parse_expr_opt(lex)?.map(Box::new);
        e::ExprKind::Return(v).to_expr(lex.end_span(&ps))
        },
    Token::RWord(ReservedWord::Continue) => {
        lex.consume();
        e::ExprKind::Continue.to_expr(lex.end_span(&ps))
        },
    Token::RWord(ReservedWord::Break) => {
        lex.consume();
        let v = parse_expr_opt(lex)?.map(Box::new);
        e::ExprKind::Break(v).to_expr(lex.end_span(&ps))
        },
    // --- loops ---
    // `loop { ... }`
    Token::RWord(ReservedWord::Loop) => {
        lex.consume();
        lex.consume_punct(Punct::BraceOpen)?;
        let body = parse_block(lex)?;
        e::ExprKind::Loop { body }.to_expr(lex.end_span(&ps))
        },
    // `for v in start .. end { ... } [else { ... }]`
    Token::RWord(ReservedWord::For) => {
        lex.consume();
        let pattern = super::parse_pattern(lex)?;
        lex.consume_rword(ReservedWord::In)?;
        let start = Box::new(parse_expr_nostruct(lex)?);
        lex.consume_punct(Punct::DoubleDot)?;
        let end = Box::new(parse_expr_nostruct(lex)?);
        lex.consume_punct(Punct::BraceOpen)?;
        let body = parse_block(lex)?;
        let else_block = if lex.opt_consume_rword(ReservedWord::Else)? {
                lex.consume_punct(Punct::BraceOpen)?;
                Some(parse_block(lex)?)
            }
            else {
                None
            };
        e::ExprKind::ForLoop {
            pattern,
            start,
            end,
            body,
            else_block,
            }.to_expr(lex.end_span(&ps))
        },
    // `while cond { ... } [else { ... }`
    Token::RWord(ReservedWord::While) => {
        lex.consume();
        let cond = Box::new(parse_expr_nostruct(lex)?);
        lex.consume_punct(Punct::BraceOpen)?;
        let body = parse_block(lex)?;
        let else_block = if lex.opt_consume_rword(ReservedWord::Else)? {
                lex.consume_punct(Punct::BraceOpen)?;
                Some(parse_block(lex)?)
            }
            else {
                None
            };
        e::ExprKind::WhileLoop {
            cond,
            body,
            else_block
        }.to_expr(lex.end_span(&ps))
        },
    // --- conditionals ---
    // `if cond { ... } [else if cond { ... }] [else { ... }]`
    Token::RWord(ReservedWord::If) => {
        lex.consume();
        let mut branches = Vec::new();
        let fallback = loop {
            let cond = parse_expr_nostruct(lex)?;
            lex.consume_punct(Punct::BraceOpen)?;
            let body = parse_block(lex)?;
            branches.push(e::IfCondition { cond, body });

            if !lex.opt_consume_rword(ReservedWord::Else)? {
                break None;
            }
            if !lex.opt_consume_rword(ReservedWord::If)? {
                lex.consume_punct(Punct::BraceOpen)?;
                break Some(parse_block(lex)?)
            }
            // `else if` consumed, continue looping
        };
        e::ExprKind::IfChain {
            branches,
            else_block: fallback,
            }.to_expr(lex.end_span(&ps))
        },
    // `match value { ... }`
    Token::RWord(ReservedWord::Match) => {
        lex.consume();
        let value = Box::new(parse_expr_nostruct(lex)?);
        let mut branches = Vec::new();
        
        lex.consume_punct(Punct::BraceOpen)?;
        loop {
            if lex.check_punct(Punct::BraceClose)? {
                break
            }
            let pat = super::parse_pattern(lex)?;
            lex.consume_punct(Punct::FatArrow)?;
            // If the item is a block, then allow the comma to be elided
            let allow_no_comma = lex.check_punct(Punct::BraceOpen)?;
            let val = parse_expr(lex)?;
            branches.push(crate::ast::expr::MatchArm {
                pat,
                val,
            });
            if !lex.opt_consume_punct(Punct::Comma)? && !allow_no_comma {
                break
            }
        }
        lex.consume_punct(Punct::BraceClose)?;

        e::ExprKind::Match {
            value,
            branches,
            }.to_expr(lex.end_span(&ps))
        },
    t => todo!("{}: parse_expr_value - {:?}", ps, t),
    })
}
