use ::proc_macro2::TokenTree;

pub struct Lexer {
    inner_iter: ::proc_macro2::token_stream::IntoIter,
    stack: Vec<StackEnt>,
    cur: Option<Token>,
}
struct StackEnt {
    iter: ::proc_macro2::token_stream::IntoIter,
    close: Option<Punct>,
}
impl Lexer {
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
        };
        rv.cur = rv.advance();
        Ok(rv)
    }

    fn advance(&mut self) -> Option<Token> {
        let rv = self.advance_inner();
        println!("advance: {:?}", rv);
        rv
    }
    fn advance_inner(&mut self) -> Option<Token> {
        // Loop, recursing into `TokenTree`s
        loop {
            let tt = loop {
                let Some(e) = self.stack.last_mut() else {
                    break self.inner_iter.next()?;
                    };
                
                if let Some(tt) = e.iter.next() {
                    break tt;
                }
                
                if let Some(rv) = self.stack.pop().unwrap().close {
                    return Some(Token::Punct(rv));
                }
            };
    
            match tt
            {
            TokenTree::Group(g) => {
                use ::proc_macro2::Delimiter;
                let (open,close) = match g.delimiter() {
                    Delimiter::None => (None, None),
                    Delimiter::Brace       => (Some(Punct::BraceOpen ), Some(Punct::BraceClose )),
                    Delimiter::Bracket     => (Some(Punct::SquareOpen), Some(Punct::SquareClose)),
                    Delimiter::Parenthesis => (Some(Punct::ParenOpen ), Some(Punct::ParenClose )),
                    };
                self.stack.push(StackEnt { iter: g.stream().into_iter(), close });
                if let Some(rv) = open {
                    return Some(Token::Punct(rv));
                }
                // Loop
                },
            TokenTree::Ident(i) => return Some(match i.to_string().parse()
                {
                Ok(v) => Token::RWord(v),
                Err(_) => Token::Ident(i),
                }),
            TokenTree::Literal(l) => return Some(Token::Literal({
                match ::litrs::Literal::from(l)
                {
                ::litrs::Literal::Bool(b) => Literal::Bool(b.value()),
                ::litrs::Literal::String(s) => Literal::String(s.value().as_bytes().to_owned()),
                l => todo!("Handle literal - {:?}", l),
                }
                })),
            TokenTree::Punct(p) if p.spacing() == ::proc_macro2::Spacing::Joint => return Some(Token::Punct({
                let n = self.stack.last_mut()
                    .map(|v| &mut v.iter)
                    .unwrap_or(&mut self.inner_iter)
                    .next()
                    .expect("Joined token but no next!");
                let TokenTree::Punct(n) = n else { panic!("Joined token but next not Punct!") };
                if n.spacing() == ::proc_macro2::Spacing::Joint {
                    let n2 = self.stack.last_mut()
                        .map(|v| &mut v.iter)
                        .unwrap_or(&mut self.inner_iter)
                        .next()
                        .expect("Joined token but no next!");
                    let TokenTree::Punct(n2) = n2 else { panic!("Joined token but next not Punct!") };
                    assert!(n2.spacing() != ::proc_macro2::Spacing::Joint);
                    Punct::from_char3(p.as_char(), n.as_char(), n2.as_char())
                }
                else {
                    Punct::from_char2(p.as_char(), n.as_char())
                }
                })),
            TokenTree::Punct(p) => return Some(Token::Punct(Punct::from_char1(p.as_char()))),
            }
        }
    }
    
    pub fn unexpected(&self) -> super::Error {
        todo!("Unexpected token error");
    }

    pub fn peek(&self) -> &Option<Token> {
        &self.cur
    }
    pub fn peek_no_eof(&self) -> super::Result<&Token> {
        match self.cur {
        Some(ref v) => Ok(v),
        None => todo!("EOF error"),
        }
    }

    pub fn consume(&mut self) -> Option<Token> {
        let rv = self.cur.take();
        self.cur = self.advance();
        rv
    }
    pub fn consume_no_eof(&mut self) -> super::Result<Token> {
        match self.cur.take() {
        Some(v) => {
            self.cur = self.advance();
            Ok(v)
            },
        None => todo!("EOF error"),
        }
    }

    pub fn check_punct(&self, exp: Punct) -> super::Result<bool> {
        match self.cur {
        Some(Token::Punct(ref p)) if *p == exp => Ok(true),
        None => Ok(false),
        Some(_) => Ok(false),
        }
    }
    pub fn consume_punct(&mut self, exp: Punct) -> super::Result<()> {
        match self.consume() {
        Some(Token::Punct(p)) if p == exp => Ok(()),
        None => todo!("EOF error"),
        Some(t) => todo!("Error: Expected {:?}, got {:?}", exp, t),
        }
    }
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
        Some(Token::RWord(ref have)) if *have == exp => Ok(true),
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
        Some(Token::Ident(_)) => Ok(Some(self.consume_ident()?)),
        None => todo!("EOF error"),
        Some(_) => Ok(None),
        }
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
    For,
    While,
    Loop,
    Match,

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
            fn from_char2(a: char, b: char) -> Punct {
                match (a,b) {
                $( ($char2_1, $char2_2) => Punct::$name2, )*
                _ => todo!("Unsupported punctuation pair: '{}{}'", a, b),
                }
            }
            fn from_char3(a: char, b: char, c: char) -> Punct {
                match (a,b,c) {
                $( ($char3_1, $char3_2, $char3_3) => Punct::$name3, )*
                _ => todo!("Unsupported punctuation triple: '{}{}{}'", a, b, c),
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
    
    Star => '*',
    Percent => '%',
    Plus => '+',
    Minus => '-',

    Amp => '&',
    Caret => '^',
    Pipe => '|',
    Equals => '=',

    ThinArrow => '-' '>',
    DoubleColon => ':' ':',
    DoubleDot => '.' '.',
    TripleDot => '.' '.' '.',
    
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
    String(Vec<u8>),
    Float(f64, Option<FloatClass>),
    Integer(bool, u128, Option<IntClass>),
}
#[derive(Debug)]
pub enum FloatClass {
    F32,
    F64,
}
#[derive(Debug)]
pub enum IntClass {
    I8, U8,
}