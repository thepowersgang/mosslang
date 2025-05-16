//! Lexer - Convert files/strings into a sequence of tokens

// /*
#[path="lex-new.rs"]
mod new;
use new::{RawLexer,PointSpan};
pub use new::{Ident,Span};
// */

/*
#[path="lex-pm2.rs"]
mod pm2;
use pm2::{RawLexer,PointSpan};
pub use pm2::{Ident,Span};
// */

#[derive(Clone)]
pub struct ProtoSpan(PointSpan);
impl ::std::fmt::Display for ProtoSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

pub struct Lexer {
    inner: RawLexer,
    cur: Option<(PointSpan, Token, PointSpan)>,
    prev_end: Option<PointSpan>,
    flags: u32,
}
impl Lexer {
    /// Create a lexer from a file
    pub fn from_path(path: &::std::path::Path) -> super::Result<Lexer> {
        let mut inner = RawLexer::new(path)?;
        Ok(Lexer {
            cur: inner.get_token().expect("IO Error?"),
            prev_end: None,
            inner,
            flags: 0,
        })
    }

    pub fn start_span(&self) -> ProtoSpan {
        ProtoSpan(self.cur.as_ref().unwrap().0.clone())
    }
    pub fn end_span(&self, ps: &ProtoSpan) -> Span {
        Span::new(ps.0.clone(), self.prev_end.as_ref().unwrap().clone())
    }

    fn advance(&mut self) -> Option<(PointSpan, Token, PointSpan)> {
        self.inner.get_token().expect("IO Error?")
    }
    
    /// Emit an error if the current token isn't expected
    #[track_caller]
    pub fn unexpected(&self) -> super::Error {
        match &self.cur {
        None => panic!("Unexpected end of file"),
        Some((sp,t,_sp_after)) => panic!("{}: Unexpected token {:?}", sp, t),
        }
    }

    /// Check the current token
    pub fn peek(&self) -> Option<&Token> {
        self.cur.as_ref().map(|v| &v.1)
    }
    /// Check the current token, returning an error if EOF is seen
    pub fn peek_no_eof(&self) -> super::Result<&Token> {
        match self.cur {
        Some((_, ref v, _)) => Ok(v),
        None => todo!("EOF error"),
        }
    }

    fn consume_with_span(&mut self) -> Option<(PointSpan,Token)> {
        let rv = self.cur.take();
        self.cur = self.advance();
        match rv {
        Some((sp, tok, end_span)) => {
            self.prev_end = Some(end_span);
            Some((sp, tok))
            },
        None => None,
        }
    }

    /// Consume the current token
    pub fn consume(&mut self) -> Option<Token> {
        self.consume_with_span().map(|v| v.1)
    }
    /// Consume the current token, returning an error if EOF is seen
    pub fn consume_no_eof(&mut self) -> super::Result<Token> {
        match self.cur.take() {
        Some((_, tok, end_span)) => {
            self.prev_end = Some(end_span);
            self.cur = self.advance();
            Ok(tok)
            },
        None => todo!("EOF error"),
        }
    }

    /// Check if the current token is a given punctuation token
    pub fn check_punct(&self, exp: Punct) -> super::Result<bool> {
        match self.cur {
        Some((_, Token::Punct(ref p),_)) if *p == exp => Ok(true),
        None => Ok(false),
        Some(_) => Ok(false),
        }
    }
    /// Consume the current token, asserting that it is a given punctuation token
    #[track_caller]
    pub fn consume_punct(&mut self, exp: Punct) -> super::Result<()> {
        match self.consume_with_span() {
        Some((_, Token::Punct(p))) if p == exp => Ok(()),
        None => todo!("EOF error"),
        Some((sp, t)) => todo!("{}: Expected {:?}, got {:?}", sp, exp, t),
        }
    }
    /// Check if the current token is a given punctuation token, consuming if it is
    pub fn opt_consume_punct(&mut self, exp: Punct) -> super::Result<bool> {
        if self.check_punct(exp)? {
            self.consume();
            Ok(true)
        }
        else {
            Ok(false)
        }
    }
    

    pub fn check_rword(&self, exp: ReservedWord) -> super::Result<bool> {
        match self.cur {
        Some((_, Token::RWord(ref have),_)) if *have == exp => Ok(true),
        None => todo!("EOF error"),
        Some(_) => Ok(false),
        }
    }
    #[track_caller]
    pub fn consume_rword(&mut self, exp: ReservedWord) -> super::Result<()> {
        match self.consume_with_span() {
        Some((_, Token::RWord(have))) if have == exp => Ok(()),
        None => todo!("EOF error"),
        Some((sp, t)) => todo!("{}: Expected {:?}, got {:?}", sp, exp, t),
        }
    }
    pub fn opt_consume_rword(&mut self, exp: ReservedWord) -> super::Result<bool> {
        if self.check_rword(exp)? {
            self.consume();
            Ok(true)
        }
        else {
            Ok(false)
        }
    }

    pub fn consume_ident(&mut self) -> super::Result<Ident> {
        match self.consume_with_span() {
        Some((_, Token::Ident(i))) => Ok(i),
        None => todo!("EOF error"),
        Some((sp, t)) => todo!("{sp}: Expected ident, got {:?}", t),
        }
    }
    pub fn opt_consume_ident(&mut self) -> super::Result<Option<Ident>> {
        match self.cur {
        Some((_, Token::Ident(_),_)) => Ok(Some(self.consume_ident()?)),
        None => todo!("EOF error"),
        Some(_) => Ok(None),
        }
    }

    #[track_caller]
    pub fn consume_string(&mut self) -> super::Result< crate::ast::StringLiteral > {
        match self.consume_with_span() {
        Some((_,Token::Literal(Literal::String(i)))) => Ok(i),
        None => todo!("EOF error"),
        Some((sp, t)) => todo!("{sp}: Expected ident, got {:?}", t),
        }
    }
    pub fn opt_consume_string(&mut self) -> super::Result<Option< crate::ast::StringLiteral >> {
        match self.cur {
        Some((_, Token::Literal(Literal::String(_)),_)) => Ok(Some(self.consume_string()?)),
        None => todo!("EOF error"),
        Some(_) => Ok(None),
        }
    }
}

pub enum Flag {
    NoStructLiteral,
}
impl Lexer
{
    pub fn has_flag(&self, flag: Flag) -> bool {
        self.flags & 1 << (flag as u32) != 0
    }
    pub fn set_flag(&mut self, flag: Flag, val: bool) -> bool {
        let b = 1 << (flag as u32);
        let rv = self.flags & b != 0;
        if val {
            self.flags |= b;
        }
        else {
            self.flags &= !b;
        }
        rv
    }
}

#[derive(Debug)]
pub enum Token {
    Ident(Ident),
    RWord(ReservedWord),
    Punct(Punct),
    Literal(Literal),
}
#[derive(Debug,PartialEq)]
#[derive(::strum::EnumString)]
#[strum(serialize_all = "snake_case")]
pub enum ReservedWord {
    Type,
    Struct,
    Union,
    Enum,
    Extern,

    Const,
    Mut,
    Static,
    Fn,
    Let,
    If,
    Else,
    For,
    While,
    Loop,
    Match,

    In,
    Return,
    Break,
    Continue,

    As,

    #[strum(to_string="self")]
    Self_,
    Super,

    Sizeof,
    Typeof,
}
macro_rules! define_punct {
    ( $( $name:ident => $($l:literal)* ),* $(,)? ) => {
        define_punct!(@inner $($name),* : () : () : () : ( $( $name => ($($l)*), )* ) );
    };
    // Handle no-matching: `Foo => ,`
    ( @inner $( $name:ident ),* : ( $( $tt_1:tt)* ) : ( $( $tt_2:tt)* ) : ( $( $tt_3:tt)* ) : ( $name_e:ident => (), $($tt:tt)* ) ) => {
        define_punct!( @inner $( $name ),* : ($( $tt_1 )*) : ($( $tt_2 )*) : ($( $tt_3 )*) : ( $($tt)* ) );
    };
    // Handle singles: `Bar => 'a'`
    ( @inner $( $name:ident ),* : ( $( $tt_1:tt)* ) : ( $( $tt_2:tt)* ) : ( $( $tt_3:tt)* ) : ( $name1_e:ident => ($char1_1_e:literal), $($tt:tt)* ) ) => {
        define_punct!( @inner $( $name ),*
            : ( $name1_e => ($char1_1_e), $($tt_1)* )
            : ( $($tt_2)* )
            : ( $($tt_3)* )
            : ( $($tt)* ) );
    };
    // Handle doubles: `Baz => 'a' 'b'`
    ( @inner $( $name:ident ),* : ( $( $tt_1:tt)* ) : ( $( $tt_2:tt)* ) : ( $( $tt_3:tt)* ) : ( $name2_e:ident => ($char2_1_e:literal $char2_2_e:literal), $($tt:tt)* ) ) => {
        define_punct!( @inner $( $name ),*
            : ( $($tt_1)* )
            : ( $name2_e => ($char2_1_e, $char2_2_e), $($tt_2)* )
            : ( $($tt_3)* )
            : ( $($tt)* ) );
    };
    // Handle triples: `Baz => 'a' 'b' 'c'`
    ( @inner $( $name:ident ),* : ( $( $tt_1:tt)* ) : ( $( $tt_2:tt)* ) : ( $( $tt_3:tt)* ) : ( $name_e:ident => ($char_1_e:literal $char_2_e:literal $char_3_e:literal), $($tt:tt)* ) ) => {
        define_punct!( @inner $( $name ),*
            : ( $($tt_1)* )
            : ( $($tt_2)* )
            : ( $name_e => ($char_1_e, $char_2_e, $char_3_e), $($tt_3)* )
            : ( $($tt)* ) );
    };
    // Final state: Nothing left in final parens
    ( @inner $( $name:ident ),*
        : ( $( $name1:ident => ( $char1_1:literal ), )* )
        : ( $( $name2:ident => ( $char2_1:literal, $char2_2:literal ), )* )
        : ( $( $name3:ident => ( $char3_1:literal, $char3_2:literal, $char3_3:literal ), )* )
        : ()
    ) => {
        #[derive(Debug,PartialEq)]
        pub enum Punct {
            $($name),*
        }
        impl Punct {
            fn from_char1(c: char) -> Option<Punct> {
                match c {
                $( $char1_1 => Some(Punct::$name1), )*
                _ => None,
                }
            }
            fn from_char2(a: char, b: char) -> Option<Punct> {
                match (a,b) {
                $( ($char2_1, $char2_2) => Some(Punct::$name2), )*
                _ => None,
                }
            }
            fn from_char3(a: char, b: char, c: char) -> Option<Punct> {
                match (a,b,c) {
                $( ($char3_1, $char3_2, $char3_3) => Some(Punct::$name3), )*
                _ => None,
                }
            }
        }
    };
}
define_punct!{
    Hash => '#',
    Bang => '!',
    Slash => '/',
    Colon => ':',
    Semicolon => ';',
    Dot => '.',
    Comma => ',',
    At => '@',

    Lt => '<',
    Gt => '>',
    
    Star => '*',
    Percent => '%',
    Plus => '+',
    Minus => '-',

    Amp => '&',
    Caret => '^',
    Pipe => '|',
    Equals => '=',
    
    PlusEquals   => '+' '=',
    MinusEquals  => '-' '=',
    SlashEquals  => '/' '=',
    StarEquals   => '*' '=',
    PercentEquals => '%' '=',
    PipeEquals => '|' '=',
    AmpEquals  => '&' '=',
    CaretEquals  => '^' '=',
    DoubleLtEquals => '<' '<' '=',
    DoubleGtEquals => '>' '>' '=',

    ThinArrow => '-' '>',
    FatArrow => '=' '>',
    DoubleColon => ':' ':',
    DoubleDot => '.' '.',
    TripleDot => '.' '.' '.',

    DoubleLt => '<' '<',
    DoubleGt => '>' '>',
    DoublePipe => '|' '|',
    DoubleAmp => '&' '&',
    
    DoubleEqual => '=' '=',
    ExlamEqual => '!' '=',
    LtEqual => '<' '=',
    GtEqual => '>' '=',
    
    ParenOpen =>,
    ParenClose =>,
    SquareOpen =>,
    SquareClose =>,
    BraceOpen =>,
    BraceClose =>,
}

#[derive(Debug)]
pub enum Literal {
    Bool(bool),
    String(crate::ast::StringLiteral),
    Float(f64, Option<FloatClass>),
    Integer(u128, Option<IntClass>),
}
#[derive(Debug)]
pub enum FloatClass {
    F32,
    F64,
}
#[derive(Debug)]
pub enum IntClass {
    Pointer,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
}