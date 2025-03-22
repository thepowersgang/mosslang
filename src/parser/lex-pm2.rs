//! 
//! This wraps `proc_macro2`'s lexer, as the idea is to use the same sort of language design
use ::proc_macro2::TokenTree;
use super::{Token,Punct,Literal,IntClass,FloatClass};
pub use proc_macro2::Ident;

#[derive(Copy,Clone)]
pub struct PointSpan(::proc_macro2::Span);
impl ::std::fmt::Display for PointSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: ", self.0)
    }
}

#[derive(Debug,Copy,Clone)]
pub struct Span(::proc_macro2::Span, ::proc_macro2::Span, );
impl Span {
    pub(super) fn new(start: PointSpan, end: PointSpan) -> Self {
        Span(start.0, end.0)
    }
}
impl ::std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}-{:?}: ", self.0, self.1)
    }
}

pub struct RawLexer
{
    inner_iter: ::proc_macro2::token_stream::IntoIter,
    stack: Vec<StackEnt>,
    pushback_stack: Vec<TokenTree>,
}
struct StackEnt {
    iter: ::proc_macro2::token_stream::IntoIter,
    close: Option<(Punct,::proc_macro2::Span,)>,
}
impl RawLexer {
    pub fn new(path: &::std::path::Path) -> ::std::io::Result<Self> {
        let mut fp = ::std::fs::File::open(path)?;
        let mut buf = String::new();
        ::std::io::Read::read_to_string(&mut fp, &mut buf)?;
        let lexed: ::proc_macro2::TokenStream = match buf.parse()
            {
            Ok(v) => v,
            Err(e) => panic!("{:?}", e),
            };
        Ok(RawLexer {
            inner_iter: lexed.into_iter(),
            stack: Default::default(),
            pushback_stack: Vec::new(),
        })
    }

    pub fn get_token(&mut self) -> ::std::io::Result< Option<(PointSpan,Token,PointSpan)> > {
        Ok(match self.advance_inner() {
        None => None,
        Some((sp,t)) => Some((PointSpan(sp), t, PointSpan(sp))),
        })
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
                ::litrs::Literal::Float(v) => {
                    let cls = match v.suffix() {
                        "f32" => Some(FloatClass::F32),
                        "f64" => Some(FloatClass::F64),
                        s => todo!("Float suffix {:?}", s),
                        };
                    let v: f64 = v.number_part().parse().unwrap();
                    Literal::Float(v, cls)
                    }
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
                        Punct::from_char1(p.as_char()).unwrap_or_else(|| todo!("Unsupported punctuation character: '{}'", p.as_char()))
                    }
                }
                else {
                    if let Some(v) = Punct::from_char2(p.as_char(), n.as_char()) {
                        v
                    }
                    else {
                        self.pushback_stack.push(TokenTree::Punct(n));
                        Punct::from_char1(p.as_char()).unwrap_or_else(|| todo!("Unsupported punctuation character: '{}'", p.as_char()))
                    }
                }
                }))),
            TokenTree::Punct(p) => return Some((p.span(), Token::Punct({
                Punct::from_char1(p.as_char()).unwrap_or_else(|| todo!("Unsupported punctuation character: '{}'", p.as_char()))
                }),)),
            }
        }
    }
}