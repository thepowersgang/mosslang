
use super::lex::{Token, ReservedWord, Punct};
use crate::ast::expr as e;

pub fn parse_root_block(lex: &mut super::Lexer) -> super::Result<crate::ast::ExprRoot> {
    lex.consume_punct(Punct::BraceOpen)?;
    let b = parse_block(lex)?;
    Ok(crate::ast::ExprRoot {
        e: e::Expr::Block(b),
    })
}
pub fn parse_root_expr(lex: &mut super::Lexer) -> super::Result<crate::ast::ExprRoot> {
    let e = parse_expr(lex)?;
    Ok(crate::ast::ExprRoot {
        e,
    })
}

fn parse_block(lex: &mut super::Lexer) -> super::Result<e::Block> {
    let mut stmts = Vec::new();
    let end = loop {
        println!("parse_block: start {:?}", lex.peek());
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
                    crate::ast::Type::new_infer()
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
    //println!("parse_block: done {:?}", lex.peek());
    lex.consume_punct(Punct::BraceClose)?;
    Ok(e::Block {
        statements: stmts,
        result: end.map(Box::new),
    })
}

fn parse_expr(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    parse_expr_assign(lex)
}

// Precedence
macro_rules! def_left_assoc {
    ($name:ident, $next:ident, $v1:ident, $v2:ident => { $( $p:ident => $e:expr ),* }) => {
        fn $name(lex: &mut super::Lexer) -> super::Result<e::Expr> {
            #[allow(unused_mut)]
            let mut $v1 = $next(lex)?;
            loop {
                match lex.peek_no_eof()?
                {
                $( Token::Punct(Punct::$p) => {
                    lex.consume();
                    let $v2 = $next(lex)?;
                    $v1 = $e
                    }),*
                _ => break,
                }
            }
            Ok($v1)
        }

    }
}
macro_rules! def_binops {
    ( $($name:ident { $($p:ident => $op:ident),* });* ; -> $next:ident  ) => {
        def_binops!{ @outer $($name )* $next; $({ $($p => $op),* })* }
    };
    ( @outer $next:ident ; ) => {};
    ( @outer $name:ident $next:ident $($others:ident)*; $blk:tt $($other_blocks:tt)* ) => {
        def_binops!{ @inner $name,$next $blk }
        def_binops!{ @outer $next $($others)*; $($other_blocks)* }
    };
    ( @inner $name:ident,$next:ident { $($p:ident => $op:ident),* } ) => {
        def_left_assoc!{$name, $next, v1,v2 => {
            $( $p  => e::Expr::BinOp(e::BinOpTy::$op, Box::new(v1), Box::new(v2)) ),*
        }}
    };
}
// - Assignment
fn parse_expr_assign(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    let lhs = parse_expr_eq(lex)?;
    Ok(lhs)
}
// - Ranges
def_binops!{
    // - Boolean OR, Boolean AND
    //parse_expr_bool_or { DoublePipe => BoolOr };
    // - Equalities
    parse_expr_eq { };
    // - Orderings
    parse_expr_cmp { };
    // - Biwise (OR, XOR, AND)
    parse_expr_bitor  { Pipe  => BitOr  };
    parse_expr_bitxor { Caret => BitXor };
    parse_expr_bitand { Amp   => BitAnd };
    // - Shifts
    parse_expr_shift { };
    // - Add / Subtract
    parse_expr_add { Plus => Add, Minus => Sub };
    // - Mul / Div / Rem
    parse_expr_mul { Star => Mul, Slash => Div, Percent => Rem };
    -> parse_expr_cast
}
// - Cast (can't use the binop code, as it's a reserved word)
fn parse_expr_cast(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    let mut v = parse_expr_leading(lex)?;
    loop {
        if lex.opt_consume_rword(ReservedWord::As)? {
            let ty = super::parse_type(lex)?;
            v = e::Expr::Cast(Box::new(v), ty);
        }
        else {
            break;
        }
    }
    Ok(v)
}
// - Leading unary ops
fn parse_expr_leading(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    Ok(if lex.opt_consume_punct(Punct::Star)? {
        e::Expr::Deref(Box::new(parse_expr_leading(lex)?))
    }
    else if lex.opt_consume_punct(Punct::Bang)? {
        e::Expr::UniOp(e::UniOpTy::Invert, Box::new(parse_expr_leading(lex)?))
    }
    else {
        parse_expr_trailing(lex)?
    })
}
// - Trailing unaries (call, index, field)
fn parse_expr_trailing(lex: &mut super::Lexer) -> super::Result<e::Expr> {
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
            v = e::Expr::CallValue(Box::new(v), args)
        }
        // indexing - `(...)[...]`
        else if lex.opt_consume_punct(Punct::SquareOpen)? {
            let i = parse_expr(lex)?;
            lex.consume_punct(Punct::SquareClose)?;
            v = e::Expr::Index(Box::new(v), Box::new(i));
        }
        // Field access/deref
        else if lex.opt_consume_punct(Punct::Dot)? {
            // Trailing deref - `(...).*`
            if lex.opt_consume_punct(Punct::Star)? {
                // Trailing deref
                v = e::Expr::Deref(Box::new(v));
            }
            // Named field - `(...).fieldname`
            else if let Some(i) = lex.opt_consume_ident()? {
                v = e::Expr::FieldNamed(Box::new(v), i);
            }
            // Unnamed field - `(...).123`
            else if let &Token::Literal(super::lex::Literal::Integer(false, i, None)) = lex.peek_no_eof()? {
                lex.consume();
                v = e::Expr::FieldIndex(Box::new(v), i);
            }
            else {
                todo!("parse_expr_trailing - unknown");
            }
        }
        else {
            break;
        }
    }
    println!("parse_expr_trailing: {:?}", lex.peek());
    Ok(v)
}

fn parse_expr_value(lex: &mut super::Lexer) -> super::Result<e::Expr> {
    if let Some(p) = super::opt_parse_path(lex)? {
        return Ok(if lex.opt_consume_punct(Punct::BraceOpen)? {
            todo!("parse_expr_value - struct literal")
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
            println!("{:?}", lex.peek());
            e::Expr::CallPath(p, args)
        }
        else {
            e::Expr::NamedValue(p)
        })
    }
    Ok(match lex.peek_no_eof()?
    {
    Token::Literal(_) => {
        let Token::Literal(lit) = lex.consume().unwrap() else { unreachable!() };
        match lit
        {
        super::lex::Literal::String(v) => e::Expr::LiteralString(v),
        _ => todo!("parse_expr_value - {:?}", lit),
        }
        },
    Token::Punct(Punct::ParenOpen) => {
        lex.consume();
        let rv = parse_expr(lex)?;
        lex.check_punct(Punct::ParenClose)?;
        rv
        },
    Token::Punct(Punct::BraceOpen) => {
        lex.consume();
        e::Expr::Block(parse_block(lex)?)
        },
    t => todo!("parse_expr_value - {:?}", t),
    })
}
