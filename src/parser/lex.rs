//! Lexer - Convert files/strings into a sequence of tokens
//! 
//! This wraps `proc_macro2`'s lexer, as the idea is to use the same sort of language design
use ::proc_macro2::TokenTree;

#[derive(Copy,Clone)]
pub struct ProtoSpan(::proc_macro2::Span);

pub struct Lexer {
    inner_iter: ::proc_macro2::token_stream::IntoIter,
    stack: Vec<StackEnt>,
    cur: Option<(::proc_macro2::Span,Token,)>,
    last_span: Option<::proc_macro2::Span>,
    flags: u32,
    pushback_stack: Vec<TokenTree>,
}
struct StackEnt {
    iter: ::proc_macro2::token_stream::IntoIter,
    close: Option<(Punct,::proc_macro2::Span,)>,
}
impl Lexer {
    /// Create a lexer from a file
    pub fn from_path(path: &::std::path::Path) -> super::Result<Lexer> {
        let mut fp = ::std::fs::File::open(path)?;
        let mut buf = String::new();
        ::std::io::Read::read_to_string(&mut fp, &mut buf)?;
        let lexed: ::proc_macro2::TokenStream = match buf.parse()
            {
            Ok(v) => v,
            Err(e) => panic!("{:?}", e),
            };
        let mut rv = Lexer {
            inner_iter: lexed.into_iter(),
            stack: Default::default(),
            cur: None,
            last_span: None,
            flags: 0,
            pushback_stack: Vec::new(),
        };
        rv.cur = rv.advance();
        Ok(rv)
    }

    pub fn start_span(&self) -> ProtoSpan {
        ProtoSpan(self.cur.as_ref().unwrap().0)
    }
    pub fn end_span(&self, ps: ProtoSpan) -> crate::Span {
        crate::Span(ps.0, *self.last_span.as_ref().unwrap())
    }

    /// Advance the iner state of the lexer
    fn advance(&mut self) -> Option<(::proc_macro2::Span, Token)> {
        if let Some((span,_)) = &self.cur {
            self.last_span = Some(*span);
        }
        let rv = self.advance_inner();
        println!("advance: {:?}", rv);
        rv
    }

    fn next_punct(&mut self) -> ::proc_macro2::Punct {
        let n =  if let Some(t) = self.pushback_stack.pop() {
                t
            }
            else {
                self.stack.last_mut()
                    .map(|v| &mut v.iter)
                    .unwrap_or(&mut self.inner_iter)
                    .next()
                    .expect("Joined token but no next!")
            };
        let TokenTree::Punct(n) = n else { panic!("Joined token but next not Punct!") };
        n
    }

    /// Inner implementation of `advance` (before printing)
    fn advance_inner(&mut self) -> Option<(::proc_macro2::Span,Token)> {
        // Loop, recursing into `TokenTree`s
        loop {
            let tt = loop {
                if let Some(t) = self.pushback_stack.pop() {
                    break t;
                }
                let Some(e) = self.stack.last_mut() else {
                    break self.inner_iter.next()?;
                    };
                
                if let Some(tt) = e.iter.next() {
                    break tt;
                }
                
                if let Some((rv,span)) = self.stack.pop().unwrap().close {
                    return Some((span, Token::Punct(rv)));
                }
            };
    
            match tt
            {
            TokenTree::Group(g) => {
                use ::proc_macro2::Delimiter;
                let (open,close) = match g.delimiter() {
                    Delimiter::None => (None, None),
                    Delimiter::Brace       => (Some(Punct::BraceOpen ), Some((Punct::BraceClose , g.span_close()))),
                    Delimiter::Bracket     => (Some(Punct::SquareOpen), Some((Punct::SquareClose, g.span_close()))),
                    Delimiter::Parenthesis => (Some(Punct::ParenOpen ), Some((Punct::ParenClose , g.span_close()))),
                    };
                self.stack.push(StackEnt { iter: g.stream().into_iter(), close });
                if let Some(rv) = open {
                    return Some((g.span_open(), Token::Punct(rv),));
                }
                // Loop
                },
            TokenTree::Ident(i) => return Some((i.span(), match i.to_string().parse()
                {
                Ok(v) => Token::RWord(v),
                Err(_) => Token::Ident(i),
                })),
            TokenTree::Literal(l) => return Some((l.span(), Token::Literal({
                match ::litrs::Literal::from(l)
                {
                ::litrs::Literal::Bool(b) => Literal::Bool(b.value()),
                ::litrs::Literal::String(s) => Literal::String(crate::ast::StringLiteral::from_bytes(s.value().as_bytes().to_owned())),
                ::litrs::Literal::Integer(i) => {
                    let cls = match i.suffix() {
                        "" => None,
                        "p" => Some(IntClass::Pointer),
                        "i8"  => Some(IntClass::I8 ), "u8"  => Some(IntClass::U8),
                        "i16" => Some(IntClass::I16), "u16" => Some(IntClass::U16),
                        "i32" => Some(IntClass::I32), "u32" => Some(IntClass::U32),
                        "i64" => Some(IntClass::I64), "u64" => Some(IntClass::U64),
                        s => todo!("Integer suffix {:?}", s),
                        };
                    Literal::Integer(i.value().unwrap(), cls)
                    },
                ::litrs::Literal::Byte(b) => Literal::Integer(b.value() as _, Some(IntClass::U8)),
                ::litrs::Literal::Char(b) => Literal::Integer(b.value() as _, Some(IntClass::U8)),
                l => todo!("Handle literal - {:?}", l),
                }
                }),)),
            TokenTree::Punct(p) if p.spacing() == ::proc_macro2::Spacing::Joint => return Some((p.span(),Token::Punct({
                let n = self.next_punct();
                if n.spacing() == ::proc_macro2::Spacing::Joint {
                    let n2 = self.next_punct();
                    assert!(n2.spacing() != ::proc_macro2::Spacing::Joint);
                    if let Some(v) = Punct::from_char3(p.as_char(), n.as_char(), n2.as_char()) {
                        v
                    }
                    else if let Some(v) = Punct::from_char2(p.as_char(), n.as_char()) {
                        self.pushback_stack.push(TokenTree::Punct(n2));
                        v
                    }
                    else {
                        self.pushback_stack.push(TokenTree::Punct(n2));
                        self.pushback_stack.push(TokenTree::Punct(n));
                        Punct::from_char1(p.as_char())
                    }
                }
                else {
                    if let Some(v) = Punct::from_char2(p.as_char(), n.as_char()) {
                        v
                    }
                    else {
                        self.pushback_stack.push(TokenTree::Punct(n));
                        Punct::from_char1(p.as_char())
                    }
                }
                }))),
            TokenTree::Punct(p) => return Some((p.span(), Token::Punct(Punct::from_char1(p.as_char())),)),
            }
        }
    }
    
    /// Emit an error if the current token isn't expected
    pub fn unexpected(&self) -> super::Error {
        todo!("Unexpected token error");
    }

    /// Check the current token
    pub fn peek(&self) -> Option<&Token> {
        self.cur.as_ref().map(|v| &v.1)
    }
    /// Check the current token, returning an error if EOF is seen
    pub fn peek_no_eof(&self) -> super::Result<&Token> {
        match self.cur {
        Some((_, ref v)) => Ok(v),
        None => todo!("EOF error"),
        }
    }

    /// Consume the current token
    pub fn consume(&mut self) -> Option<Token> {
        let rv = self.cur.take();
        self.cur = self.advance();
        match rv {
        Some((span, tok)) => {
            self.last_span = Some(span);
            Some(tok)
            },
        None => None,
        }
    }
    /// Consume the current token, returning an error if EOF is seen
    pub fn consume_no_eof(&mut self) -> super::Result<Token> {
        match self.cur.take() {
        Some((span, v)) => {
            self.last_span = Some(span);
            self.cur = self.advance();
            Ok(v)
            },
        None => todo!("EOF error"),
        }
    }

    /// Check if the current token is a given punctuation token
    pub fn check_punct(&self, exp: Punct) -> super::Result<bool> {
        match self.cur {
        Some((_, Token::Punct(ref p))) if *p == exp => Ok(true),
        None => Ok(false),
        Some(_) => Ok(false),
        }
    }
    /// Consume the current token, asserting that it is a given punctuation token
    pub fn consume_punct(&mut self, exp: Punct) -> super::Result<()> {
        match self.consume() {
        Some(Token::Punct(p)) if p == exp => Ok(()),
        None => todo!("EOF error"),
        Some(t) => todo!("Error: Expected {:?}, got {:?}", exp, t),
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
        Some((_, Token::RWord(ref have))) if *have == exp => Ok(true),
        None => todo!("EOF error"),
        Some(_) => Ok(false),
        }
    }
    pub fn consume_rword(&mut self, exp: ReservedWord) -> super::Result<()> {
        match self.consume() {
        Some(Token::RWord(have)) if have == exp => Ok(()),
        None => todo!("EOF error"),
        Some(t) => todo!("Error: Expected {:?}, got {:?}", exp, t),
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

    pub fn consume_ident(&mut self) -> super::Result<::proc_macro2::Ident> {
        match self.consume() {
        Some(Token::Ident(i)) => Ok(i),
        None => todo!("EOF error"),
        Some(t) => todo!("Error: Expected ident, got {:?}", t),
        }
    }
    pub fn opt_consume_ident(&mut self) -> super::Result<Option<::proc_macro2::Ident>> {
        match self.cur {
        Some((_, Token::Ident(_))) => Ok(Some(self.consume_ident()?)),
        None => todo!("EOF error"),
        Some(_) => Ok(None),
        }
    }

    pub fn consume_string(&mut self) -> super::Result< crate::ast::StringLiteral > {
        match self.consume() {
        Some(Token::Literal(Literal::String(i))) => Ok(i),
        None => todo!("EOF error"),
        Some(t) => todo!("Error: Expected ident, got {:?}", t),
        }
    }
    pub fn opt_consume_string(&mut self) -> super::Result<Option< crate::ast::StringLiteral >> {
        match self.cur {
        Some((_, Token::Literal(Literal::String(_)))) => Ok(Some(self.consume_string()?)),
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
    Ident(::proc_macro2::Ident),
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
            fn from_char1(c: char) -> Punct {
                match c {
                $( $char1_1 => Punct::$name1, )*
                _ => todo!("Unsupported punctuation character: '{}'", c),
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
    
    PlusEquals => '+' '=',
    MinusEquals => '-' '=',
    SlashEquals => '/' '=',
    StarEquals => '*' '=',
    PercentEquals => '%' '=',

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