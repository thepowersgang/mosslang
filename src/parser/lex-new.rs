//! Entirely custom lexer

use super::{Token,Punct};

static INTERNER: ::std::sync::Mutex<Option<::string_interner::DefaultStringInterner>> = ::std::sync::Mutex::new(None);
#[derive(Clone,Hash)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
pub struct Ident(::string_interner::DefaultSymbol);
impl Ident {
    pub fn len(&self) -> usize {
        match INTERNER.lock().unwrap().get_or_insert_default().resolve(self.0) {
        Some(v) => v.len(),
        _ => 0,
        }

    }
}
impl PartialEq<&'_ str> for Ident {
    fn eq(&self, other: &&'_ str) -> bool {
        match INTERNER.lock().unwrap().get_or_insert_default().resolve(self.0) {
        Some(v) if v == *other => true,
        _ => false,
        }
    }
}
impl ::std::fmt::Debug for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}#{}",
            INTERNER.lock().unwrap().get_or_insert_default().resolve(self.0).unwrap_or("?"),
            ::string_interner::Symbol::to_usize(self.0)
        )
    }
}
impl ::std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str( INTERNER.lock().unwrap().get_or_insert_default().resolve(self.0).unwrap_or("-BADSYM-") )
    }
}

/// Underlying lexer with no helpers at all
pub struct RawLexer
{
    source: ::utf8reader::UTF8Reader< ::std::io::BufReader< ::std::fs::File> >,
    path: ::std::sync::Arc<::std::path::Path>,
    cur_char: Option<char>,
    cur_line: usize,
    cur_ofs: usize,
    prev: Option<(Option<char>,usize,usize)>,
    next: Option<char>,
}

#[derive(Clone,Debug)]
pub struct PointSpan
{
    path: ::std::sync::Arc<::std::path::Path>,
    line: usize,
    ofs: usize,
}
impl ::std::fmt::Display for PointSpan {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}: ", self.path.display(), self.line, self.ofs)
    }
}

#[derive(Clone,Debug)]
pub struct Span
{
    path: Option<::std::sync::Arc<::std::path::Path>>,
    line: usize,
    ofs: usize,
    end_line: usize,
    end_ofs: usize,
}
impl Span
{
    /// Create a span indicating that the item
    pub const fn new_null() -> Self {
        //static NULL_PATH: ::std::sync::OnceLock<::std::rc::Rc<::std::path::Path>> = ::std::sync::OnceLock::new();
        Span { path: None, line: 0, ofs: 0, end_line: 0, end_ofs: 0 }
    }
    pub(super) fn new(start: PointSpan, end: PointSpan) -> Self {
        Span {
            path: Some(start.path),
            line: start.line,
            ofs: start.ofs,
            end_line: end.line,
            end_ofs: end.ofs,
        }
    }
}
impl ::std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.path {
        None => write!(f, "NULL: "),
        Some(ref path) => {
            if self.line == self.end_line {
                write!(f, "{}:{}:{}-{}: ", path.display(), self.line, self.ofs, self.end_ofs)
            }
            else {
                write!(f, "{}:{}:{}-{}:{}: ", path.display(), self.line, self.ofs, self.end_line,self.end_ofs)
            }
        }
        }
    }
}

impl RawLexer
{
    pub fn new(path: &::std::path::Path) -> ::std::io::Result<Self> {
        let fp = ::std::io::BufReader::new(::std::fs::File::open(path)?);
        let mut source = ::utf8reader::UTF8Reader::new(fp);
        Ok(RawLexer {
            path: ::std::sync::Arc::from(path),
            cur_char: source.next().transpose()?,
            cur_line: 1,
            cur_ofs: 0,
            source,
            prev: None,
            next: None,
        })
    }

    fn cur_char(&self) -> Option<char> {
        self.cur_char
    }
    /// Advance the lexer and return the new character
    fn advance(&mut self) -> ::std::io::Result<Option<char>> {
        self.prev = Some( (self.cur_char, self.cur_line, self.cur_ofs) );
        self.cur_char = match self.next.take().map(|v| Ok(v)).or_else(|| self.source.next())
            {
            None => None,
            Some(Ok(v)) => {
                if v == '\n' {
                    self.cur_line += 1;
                    self.cur_ofs = 0;
                }
                else {
                    self.cur_ofs += 1;
                }
                Some(v)
                },
            Some(Err(e)) => return Err(e),
            };
        //println!("advance: {:?} -> {:?}", self.prev.map(|v| v.0), self.cur_char);
        Ok( self.cur_char )
    }
    fn un_advance(&mut self) {
        let Some((ch,l,o)) = self.prev.take() else { panic!("un_advance called twice?") };
        self.next = self.cur_char;
        self.cur_char = ch;
        self.cur_line = l;
        self.cur_ofs = o;
        //println!("un_advance: {:?}", ch);
    }

    fn point_span(&self) -> PointSpan {
        PointSpan {
            path: self.path.clone(),
            line: self.cur_line,
            ofs: self.cur_ofs,
        }
    }
}
impl RawLexer
{
    pub fn get_token(&mut self) -> ::std::io::Result< Option<(PointSpan,Token,PointSpan)> > {
        while let Some(true) = self.cur_char().map(|c| c.is_whitespace()) {
            self.advance()?;
        }
        let ps_begin = self.point_span();
        let c = match self.cur_char() {
                Some(v) => v,
                None => return Ok(None),
            };
        let t = match c
            {
            // Numbers
            c if c.is_ascii_digit() => self.get_token_number(c)?,
            // Strings
            '"' => self.get_token_string()?,
            '\'' => self.get_token_char_lit(false)?,
            // Byte strings/char literals
            'b' => match self.advance()?
                {
                Some('\'') => self.get_token_char_lit(true)?,
                Some('\"') => self.get_token_string()?, // Byte strings?
                _ => {
                    self.un_advance();
                    self.get_token_ident(None)?
                    },
                },

            // Identifiers
            '_' => match self.advance()?
                {
                Some(c2) if c2.is_alphanumeric() || c2 == '_' =>
                    self.get_token_ident(Some(c) )?,
                _ => {
                    // TODO: Underscore token?
                    self.un_advance();
                    self.get_token_ident(None)?
                    }
                },
            c if c.is_ascii_alphabetic() => {
                self.get_token_ident(None)?
                },
            // Slash needs some special handling
            '/' => {
                match self.advance()?
                {
                // Comments - these recurse
                Some('/') => {
                    while self.advance()?.map(|c| c == '\n') == Some(false) {
                    }
                    return self.get_token();
                    },
                Some('*') => {
                    let mut level = 0u32;
                    let mut prev = ' ';
                    loop {
                        match (prev, self.advance()?) {
                        (_, None) => break, // EOF, unexpected?
                        ('*', Some(v @ '/')) => {
                            if level == 0 {
                                break ;
                            }
                            level -= 1;
                            prev = v;
                            },
                        ('/', Some(v @ '*')) => {
                            level += 1;
                            prev = v;
                            },
                        (_, Some(v)) => {
                            prev = v;
                            },
                        }
                    }
                    self.advance()?;    // Advance past the closing slash
                    return self.get_token();
                },
                Some('=') => { self.advance()?; Token::Punct(Punct::SlashEquals) },
                _ => Token::Punct(Punct::Slash),
                }
                },
            // Brackets aren't included in the `Punct::from_char1` function
            '[' => { self.advance()?; Token::Punct(Punct::SquareOpen ) },
            ']' => { self.advance()?; Token::Punct(Punct::SquareClose) },
            '(' => { self.advance()?; Token::Punct(Punct::ParenOpen ) },
            ')' => { self.advance()?; Token::Punct(Punct::ParenClose) },
            '{' => { self.advance()?; Token::Punct(Punct::BraceOpen ) },
            '}' => { self.advance()?; Token::Punct(Punct::BraceClose) },
            c => match Punct::from_char1(c)
                {
                None => todo!("Lex {:?}", c),
                Some(p) => match self.advance()?
                    {
                    None => Token::Punct(p),
                    Some(c2) => match Punct::from_char2(c, c2)
                        {
                        None => Token::Punct(p),
                        Some(p) => match self.advance()?
                            {
                            None => Token::Punct(p),
                            Some(c3) => match Punct::from_char3(c, c2, c3)
                                {
                                None => Token::Punct(p),
                                Some(p) => { self.advance()?; Token::Punct(p) },
                                },
                            }
                        },
                    }
                },
            };
        //println!("get_token: {:?}", t);
        let ps_end = self.point_span();
        Ok(Some( (ps_begin, t, ps_end) ))
    }

    fn get_token_number(&mut self, c: char) -> ::std::io::Result<Token> {
        let mut int_part = c.to_digit(10).unwrap() as _;
        let base = if c == '0' {
                match self.advance()? {
                Some('x') => 16,
                Some('o') => 8,
                _ => {
                    self.un_advance();
                    10
                },
                }
            }
            else {
                10
            };
        loop {
            match self.advance()? {
            Some('_') => {},
            Some(c) => if let Some(v) = c.to_digit(base) {
                    int_part *= base as u128;
                    int_part += v as u128;
                } else {
                    break
                },
            _ => break,
            }
        }
        match self.cur_char() {
        Some('e') => self.get_token_float(base, int_part, false),
        Some('p') if base == 16 => self.get_token_float(base, int_part, false),
        Some(c) if c.is_ascii_alphabetic() => {
            let suffix = self.get_raw_ident(None)?;
            let cls = match &suffix[..] {
                "p" => super::IntClass::Pointer,
                "u8"  => super::IntClass::U8 , "i8"  => super::IntClass::I8,
                "u16" => super::IntClass::U16, "i16" => super::IntClass::I16,
                "u32" => super::IntClass::U32, "i32" => super::IntClass::I32,
                "u64" => super::IntClass::U64, "i64" => super::IntClass::I64,
                _ => todo!("Integer suffix: {:?}", suffix),
                };
            Ok(Token::Literal(super::Literal::Integer(int_part, Some(cls))))
            },
        // Could be a float, or could be a dotted member access
        Some('.') => match self.advance()? {
            // <int>..<?>
            Some('.') => {
                self.un_advance();
                Ok(Token::Literal(super::Literal::Integer(int_part, None)))
            }
            Some(c) if c.is_digit(base) => self.get_token_float(base, int_part, true),
            // <int> . <ident>
            Some(c) if c.is_alphabetic() || c == '_' => {
                self.un_advance();
                Ok(Token::Literal(super::Literal::Integer(int_part, None)))
            }
            _ => Ok(Token::Literal(super::Literal::Float(int_part as _, None))),
            },
        _ => Ok(Token::Literal(super::Literal::Integer(int_part, None))),
        }
    }
    fn get_token_float(&mut self, base: u32, int_part: u128, is_fraction: bool) -> ::std::io::Result<Token> {
        let mut frac_part = String::new();
        if is_fraction {
            frac_part.push( self.cur_char().unwrap() );
            loop {
                match self.advance()? {
                Some(c) if c.is_digit(base) => frac_part.push(c),
                _ => break,
                }
            }
        }
        match self.cur_char() {
        Some('e') => todo!("exponent - decimal"),
        Some('p') if base == 16 => todo!("exponent - hex"),
        _ => {},
        }
    
        let value: f64 = format!("{}{}.{}", match base {
            10 => "",
            2 => "0b",
            16 => "0x",
            _ => unreachable!(),
        }, int_part, frac_part).parse().unwrap();
        
        match self.cur_char() {
        Some(c) if c.is_ascii_alphabetic() => {
            let suffix = self.get_raw_ident(None)?;
            let cls = match &suffix[..] {
                "f32" => super::FloatClass::F32,
                "f64" => super::FloatClass::F64,
                _ => todo!("Float suffix: {:?}", suffix),
                };
            Ok(Token::Literal(super::Literal::Float(value, Some(cls))))
            },
        _ => Ok(Token::Literal(super::Literal::Float(value, None))),
        }
    }
    fn get_raw_ident(&mut self, c1: Option<char>) -> ::std::io::Result<String> {
        let mut v = String::new();
        if let Some(c) = c1 {
            v.push(c);
        }
        loop {
            match self.cur_char() {
            Some(c) if c.is_alphanumeric() || c == '_' => v.push(c),
            _ => break,
            }
            self.advance()?;
        }
        Ok(v)
    }
    fn get_token_ident(&mut self, c1: Option<char>) -> ::std::io::Result<Token> {
        let v = self.get_raw_ident(c1)?;
        if v == "true" {
            Ok(Token::Literal(super::Literal::Bool(true)))
        }
        else if v == "false" {
            Ok(Token::Literal(super::Literal::Bool(false)))
        }
        else if let Ok(v) = v.parse() {
            Ok(Token::RWord(v))
        }
        else {
            Ok(Token::Ident(Ident(INTERNER.lock().unwrap().get_or_insert_default().get_or_intern(v))))
        }
    }
    fn get_token_string(&mut self) -> ::std::io::Result<Token> {
        let s = self.get_token_string_inner('"')?;
        Ok(Token::Literal(super::Literal::String(crate::ast::StringLiteral::from_bytes(s))))
    }
    fn get_token_char_lit(&mut self, is_byte: bool) -> ::std::io::Result<Token> {
        let s = self.get_token_string_inner('\'')?;
        if is_byte {
            assert!(s.len() == 1);
        }
        else {
            // Check?
        }
        Ok(Token::Literal(super::Literal::Integer( s[0] as _, Some(super::IntClass::I8) )))
    }
    /// Read escaped string data until an ending character
    fn get_token_string_inner(&mut self, end: char) -> ::std::io::Result<Vec<u8>> {
        let mut rv = Vec::new();
        enum StrIndent {
            None,
            CountUp(u32),
            Set(u32),
            CountDown(u32, u32),
        }
        let mut indent = StrIndent::None;
        fn set_newline(indent: &mut StrIndent) {
            *indent = match *indent {
                StrIndent::None => StrIndent::CountUp(0),
                StrIndent::CountUp(c)
                |StrIndent::Set(c)
                |StrIndent::CountDown(_, c) => StrIndent::CountDown(c,c),
            };
        }
        loop {
            match self.advance_no_eof()? {
            '\\' => match self.advance_no_eof()? {
                '0' => rv.push(0),
                't' => rv.push(8),
                'n' => rv.push(10),
                'r' => rv.push(13),
                '"' => rv.push(b'"'),
                '\\' => rv.push(b'\\'),
                '\r' => {
                    // Escaped newline: Just don't emit
                    if self.advance()? == Some('\n') {
                        // Also skip 
                    }
                    else {
                        self.un_advance();
                    }
                    set_newline(&mut indent);
                }
                '\n' => {
                    // Escaped newline: Just don't emit
                    set_newline(&mut indent);
                },
                c => todo!("{}: Escape code '\\' {:?}", self.point_span(), c),
                },
            '\n' => {
                rv.push(b'\n');
                set_newline(&mut indent);
            }
            '\r' => {
                rv.push(b'\r');
                if self.advance()? == Some('\n') { 
                    rv.push(b'\n');
                }
                else {
                    self.un_advance();
                }
                set_newline(&mut indent);
            }

            ' ' => {
                match &mut indent {
                StrIndent::CountUp(c) => *c += 1,
                StrIndent::CountDown(c, t) => {
                    if *c == 0 {
                        rv.push(b' ');
                        indent = StrIndent::Set(*t);
                    }
                    else {
                        *c -= 1;
                    }
                }
                _ => rv.push(b' '),
                }
            }
            c if c == end => break,
            c => {
                indent = match indent {
                    StrIndent::None => StrIndent::None,
                    StrIndent::CountUp(c)
                    |StrIndent::Set(c)
                    |StrIndent::CountDown(_, c) => StrIndent::Set(c),
                };
                let mut buf = [0; 4];
                rv.extend(c.encode_utf8(&mut buf).bytes())
                },
            }
        }
        self.advance()?;    // Move away from the closing quote
        println!(">> {:?}", String::from_utf8_lossy(&rv));
        Ok(rv)
    }
    /// Advance, but painic if EOF is seen
    fn advance_no_eof(&mut self) -> ::std::io::Result<char> {
        match self.advance()? {
        None => todo!("Unexpected EOF"),
        Some(c) => Ok(c),
        }
    }
}
