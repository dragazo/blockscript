lalrpop_mod!(grammar, "/parser/grammar.rs");

use lalrpop_util::ParseError;
use lalrpop_util::lexer::Token;

pub type Error<'a> = ParseError<usize, Token<'a>, &'a str>;

use std::fmt::{self, Display};

struct Punctuated<'a, T>(&'a [T], &'a str);
impl<'a, T: Display> Display for Punctuated<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0 {
            [] => Ok(()),
            [x] => write!(f, "{}", x),
            [items @ .., last] => {
                for x in items {
                    write!(f, "{}{}", x, self.1)?;
                }
                write!(f, "{}", last)
            }
        }
    }
}

macro_rules! raw_span_impl {
    ($($t:ty),+) => {$(
        impl $t {
            pub fn span(&self) -> Span { self.raw_span }
        }
    )+}
}

fn clean_string(s: &str) -> String {
    assert!(s.len() >= 2);
    assert!({ let c = s.chars().next().unwrap(); c == '"' || c == '\'' });
    assert_eq!(s.chars().next().unwrap(), s.chars().rev().next().unwrap());

    let mut res = String::new();
    let mut chars = s[1..s.len()-1].chars().peekable();
    while let Some(c) = chars.next() {
        if c != '\\' {
            res.push(c);
            continue;
        }

        let escaped = match chars.next().unwrap() {
            '\\' => '\\',
            '"' => '"',
            '\'' => '\'',
            't' => '\t',
            'n' => '\n',
            x => panic!("unknown escape sequence: \\{}", x),
        };
        res.push(escaped);
    }
    res
}
fn quote_string(s: &str) -> String {
    let mut res = String::new();
    res.push('"');
    for c in s.chars() {
        match c {
            '"' => { res.push('\\'); res.push('"'); }
            '\r' => { res.push('\\'); res.push('r'); }
            '\n' => { res.push('\\'); res.push('n'); }
            '\t' => { res.push('\\'); res.push('t'); }
            _ => res.push(c),
        }
    }
    res.push('"');
    res
}

/// The range of input making up some part of the AST
/// 
/// This is given as a byte offset pair `(a, b)` such that slicing with `a..b` will retrieve the associated source code for an item.
/// This is implemented for all items in the AST.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Span(usize, usize);

#[derive(Debug, Clone)]
pub enum Item {
    Stmt(Stmt),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    VarDecl(Ident, Expr),
    Assign(LValue, Expr),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Choice { condition: Box<Expr>, a: Box<Expr>, b: Box<Expr> },

    And(Box<Expr>, Box<Expr>),

    Or(Box<Expr>, Box<Expr>),

    Equ(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),

    Less(Box<Expr>, Box<Expr>),
    LessEq(Box<Expr>, Box<Expr>),
    Great(Box<Expr>, Box<Expr>),
    GreatEq(Box<Expr>, Box<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),

    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),

    Pow(Box<Expr>, Box<Expr>),

    Pos(Box<Expr>, usize),
    Neg(Box<Expr>, usize),
    Not(Box<Expr>, usize),

    Value(Value),
}
impl Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Choice { condition, b, .. } => Span(condition.span().0, b.span().1),

            Expr::And(a, b) => Span(a.span().0, b.span().1),

            Expr::Or(a, b) => Span(a.span().0, b.span().1),

            Expr::Equ(a, b) => Span(a.span().0, b.span().1),
            Expr::Neq(a, b) => Span(a.span().0, b.span().1),

            Expr::Less(a, b) => Span(a.span().0, b.span().1),
            Expr::LessEq(a, b) => Span(a.span().0, b.span().1),
            Expr::Great(a, b) => Span(a.span().0, b.span().1),
            Expr::GreatEq(a, b) => Span(a.span().0, b.span().1),

            Expr::Add(a, b) => Span(a.span().0, b.span().1),
            Expr::Sub(a, b) => Span(a.span().0, b.span().1),

            Expr::Mul(a, b) => Span(a.span().0, b.span().1),
            Expr::Div(a, b) => Span(a.span().0, b.span().1),
            Expr::Mod(a, b) => Span(a.span().0, b.span().1),

            Expr::Pow(a, b) => Span(a.span().0, b.span().1),

            Expr::Pos(v, span_left) => Span(*span_left, v.span().1),
            Expr::Neg(v, span_left) => Span(*span_left, v.span().1),
            Expr::Not(v, span_left) => Span(*span_left, v.span().1),

            Expr::Value(v) => v.span(),
        }
    }
}
impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Choice { condition, a, b } => write!(f, "({} ? {} : {})", condition, a, b),

            Expr::And(a, b) => write!(f, "({} && {})", a, b),

            Expr::Or(a, b) => write!(f, "({} || {})", a, b),

            Expr::Equ(a, b) => write!(f, "({} == {})", a, b),
            Expr::Neq(a, b) => write!(f, "({} != {})", a, b),

            Expr::Less(a, b) => write!(f, "({} < {})", a, b),
            Expr::LessEq(a, b) => write!(f, "({} <= {})", a, b),
            Expr::Great(a, b) => write!(f, "({} > {})", a, b),
            Expr::GreatEq(a, b) => write!(f, "({} >= {})", a, b),

            Expr::Add(a, b) => write!(f, "({} + {})", a, b),
            Expr::Sub(a, b) => write!(f, "({} - {})", a, b),
            
            Expr::Mul(a, b) => write!(f, "({} * {})", a, b),
            Expr::Div(a, b) => write!(f, "({} / {})", a, b),
            Expr::Mod(a, b) => write!(f, "({} % {})", a, b),
            
            Expr::Pow(a, b) => write!(f, "({} ^ {})", a, b),

            Expr::Pos(val, _) => write!(f, "(+{})", val),
            Expr::Neg(val, _) => write!(f, "(-{})", val),
            Expr::Not(val, _) => write!(f, "(!{})", val),

            Expr::Value(val) => write!(f, "{}", val),
        }
    }
}

#[derive(Debug, Clone)]
pub enum LValue {
    Ident(Ident),
    ArrayIndex(ArrayIndex),
    KeyIndex(KeyIndex),
}

#[derive(Debug, Clone)] pub struct Ident { id: String, raw_span: Span }
#[derive(Debug, Clone)] pub struct ArrayIndex { src: Box<Expr>, index: Box<Expr>, span_right: usize }
#[derive(Debug, Clone)] pub struct KeyIndex { src: Box<Expr>, key: Box<Expr>, span_right: usize }
#[derive(Debug, Clone)] pub struct Number { value: String, raw_span: Span }
#[derive(Debug, Clone)] pub struct Text { content: String, raw_span: Span }
#[derive(Debug, Clone)] pub struct List { values: Vec<Expr>, raw_span: Span }
#[derive(Debug, Clone)] pub struct Object { fields: Vec<Field>, raw_span: Span }
#[derive(Debug, Clone)] pub struct Field { id: Text, value: Expr }

raw_span_impl! { Ident, Number, Text, List, Object }
impl ArrayIndex { pub fn span(&self) -> Span { Span(self.src.span().0, self.span_right) } }
impl KeyIndex { pub fn span(&self) -> Span { Span(self.src.span().0, self.span_right) } }
impl Field { pub fn span(&self) -> Span { Span(self.id.span().0, self.value.span().1) } }

impl Display for Ident { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.id) } }
impl Display for ArrayIndex { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "({}[{}])", self.src, self.index) } }
impl Display for KeyIndex { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "({}.[{}])", self.src, self.key) } }
impl Display for Number { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.value) } }
impl Display for Text { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", quote_string(&self.content)) } }
impl Display for List { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "[{}]", Punctuated(&self.values, ", ")) } }
impl Display for Object { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{{{}}}", Punctuated(&self.fields, ", ")) } }
impl Display for Field { fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}:{}", quote_string(&self.id.content), self.value) } }

#[derive(Debug, Clone)]
pub enum Value {
    Ident(Ident),
    ArrayIndex(ArrayIndex),
    KeyIndex(KeyIndex),
    Number(Number),
    Text(Text),
    List(List),
    Object(Object),
}
impl Value {
    fn span(&self) -> Span {
        match self {
            Value::Ident(x) => x.span(),
            Value::ArrayIndex(x) => x.span(),
            Value::KeyIndex(x) => x.span(),
            Value::Number(x) => x.span(),
            Value::Text(x) => x.span(),
            Value::List(x) => x.span(),
            Value::Object(x) => x.span(),    
        }
    }
}
impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Ident(x) => write!(f, "{}", x),
            Value::ArrayIndex(x) => write!(f, "{}", x),
            Value::KeyIndex(x) => write!(f, "{}", x),
            Value::Number(x) => write!(f, "{}", x),
            Value::Text(x) => write!(f, "{}", x),
            Value::List(x) => write!(f, "{}", x),
            Value::Object(x) => write!(f, "{}", x),
        }
    }
}

#[cfg(test)]
fn get_single_stmt(prog: &str) -> Result<Stmt, Error> {
    let r = grammar::ProgramParser::new().parse(prog)?;
    assert_eq!(r.len(), 1);
    match r.into_iter().next().unwrap() {
        Item::Stmt(s) => Ok(s),
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_parse_number() {
    match get_single_stmt("_ = 22;").unwrap() {
        Stmt::Assign(_, Expr::Value(Value::Number(num))) => {
            assert_eq!(num.value, "22");
            assert_eq!(num.span(), Span(4, 6));
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt("_ = 22.43;").unwrap() {
        Stmt::Assign(_, Expr::Value(Value::Number(num))) => {
            assert_eq!(num.value, "22.43");
            assert_eq!(num.span(), Span(4, 9));
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt("_ = +22.43e56;").unwrap() {
        Stmt::Assign(_, Expr::Pos(x, span_left)) => match *x {
            Expr::Value(Value::Number(num)) => {
                assert_eq!(num.value, "22.43e56");
                assert_eq!(num.span(), Span(5, 13));
                assert_eq!(Expr::Pos(Box::new(Expr::Value(Value::Number(num))), span_left).to_string(), "(+22.43e56)");
            }
            x => panic!("{:?}", x),
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt("_ = 22e+56;").unwrap() {
        Stmt::Assign(_, Expr::Value(Value::Number(num))) => {
            assert_eq!(num.value, "22e+56");
            assert_eq!(num.span(), Span(4, 10));
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt("_ = 0.1e-56;").unwrap() {
        Stmt::Assign(_, Expr::Value(Value::Number(num))) => {
            assert_eq!(num.value, "0.1e-56");
            assert_eq!(num.span(), Span(4, 11));
        }
        x => panic!("{:?}", x),
    }
}
#[test]
fn test_prec() {
    match get_single_stmt("_ = 22+34+3;").unwrap() {
        Stmt::Assign(_, x) => assert_eq!(format!("{}", x), "((22 + 34) + 3)"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt("_ = 1+2*3+4;").unwrap() {
        Stmt::Assign(_, x) => assert_eq!(format!("{}", x), "((1 + (2 * 3)) + 4)"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt("_ = 1+2*(3+4);").unwrap() {
        Stmt::Assign(_, x) => assert_eq!(format!("{}", x), "(1 + (2 * (3 + 4)))"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt("_ = 1+2*(3+4);").unwrap() {
        Stmt::Assign(_, x) => assert_eq!(format!("{}", x), "(1 + (2 * (3 + 4)))"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt("_ = 1+2*(-3+4);").unwrap() {
        Stmt::Assign(_, x) => assert_eq!(format!("{}", x), "(1 + (2 * ((-3) + 4)))"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt("_ = 1+2*-(-3+4);").unwrap() {
        Stmt::Assign(_, x) => assert_eq!(format!("{}", x), "(1 + (2 * (-((-3) + 4))))"),
        x => panic!("{:?}", x),
    }
}
#[test]
fn test_parse_text() {
    match get_single_stmt(r#"_ ="heloo world";"#).unwrap() {
        Stmt::Assign(_, Expr::Value(Value::Text(txt))) => {
            assert_eq!(txt.content, "heloo world");
            assert_eq!(txt.span(), Span(3, 16));
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ ="heloo \"world";"#).unwrap() {
        Stmt::Assign(_, Expr::Value(Value::Text(txt))) => {
            assert_eq!(txt.content, "heloo \"world");
            assert_eq!(txt.span(), Span(3, 18));
        }
        x => panic!("{:?}", x),
    }
    get_single_stmt(r#"_ ="heloo \"world"#).unwrap_err();
    get_single_stmt(r#"_ ="heloo \"world;"#).unwrap_err();
    get_single_stmt(r#"_ ="heloo \""#).unwrap_err();
    get_single_stmt(r#"_ ="heloo \";"#).unwrap_err();
    get_single_stmt(r#"_ ="heloo world
    ";"#).unwrap_err();
}
#[test]
fn test_parse_list() {
    match get_single_stmt(r#"_ = [];"#).unwrap() {
        Stmt::Assign(_, Expr::Value(Value::List(list))) => {
            assert_eq!(list.values.len(), 0);
            assert_eq!(list.span(), Span(4, 6));
            assert_eq!(list.to_string(), "[]");
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = [   "hello "];"#).unwrap() {
        Stmt::Assign(_, Expr::Value(Value::List(list))) => {
            assert_eq!(list.values.len(), 1);
            assert_eq!(list.span(), Span(4, 17));
            match &list.values[0] {
                Expr::Value(Value::Text(txt)) => {
                    assert_eq!(txt.content, "hello ");
                    assert_eq!(txt.span(), Span(8, 16));
                }
                x => panic!("{:?}", x),
            }
            assert_eq!(list.to_string(), "[\"hello \"]");
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = [   [],];"#).unwrap() {
        Stmt::Assign(_, Expr::Value(Value::List(list))) => {
            assert_eq!(list.values.len(), 1);
            assert_eq!(list.span(), Span(4, 12));
            match &list.values[0] {
                Expr::Value(Value::List(list)) => {
                    assert_eq!(list.values.len(), 0);
                    assert_eq!(list.span(), Span(8, 10));
                }
                x => panic!("{:?}", x),
            }
            assert_eq!(Expr::Value(Value::List(list)).to_string(), "[[]]");
        }
        x => panic!("{:?}", x),
    }
    get_single_stmt(r#"_ = [,];"#).unwrap_err();
    get_single_stmt(r#"_ = ["#).unwrap_err();
    get_single_stmt(r#"_ = [;"#).unwrap_err();
    get_single_stmt(r#"_ = [7"#).unwrap_err();
    get_single_stmt(r#"_ = [7;"#).unwrap_err();
    get_single_stmt(r#"_ = [7,"#).unwrap_err();
    get_single_stmt(r#"_ = [7,;"#).unwrap_err();
}
#[test]
fn test_parse_object() {
    match get_single_stmt(r#"_ = {};"#).unwrap() {
        Stmt::Assign(_, Expr::Value(Value::Object(obj))) => {
            assert_eq!(obj.fields.len(), 0);
            assert_eq!(obj.span(), Span(4, 6));
            assert_eq!(format!("{}", Value::Object(obj)), r"{}");
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = {foo: 5};"#).unwrap() {
        Stmt::Assign(_, Expr::Value(Value::Object(obj))) => {
            assert_eq!(obj.fields.len(), 1);
            assert_eq!(obj.fields[0].id.content, "foo");
            assert_eq!(format!("{}", obj.fields[0].value), "5");
            assert_eq!(format!("{}", Value::Object(obj)), r#"{"foo":5}"#);
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = {foo: [], "bar\"":"merp\n"};"#).unwrap() {
        Stmt::Assign(_, Expr::Value(Value::Object(obj))) => {
            assert_eq!(obj.span(), Span(4, 31));
            assert_eq!(obj.fields.len(), 2);
            assert_eq!(obj.fields[0].id.content, "foo");
            assert_eq!(format!("{}", obj.fields[0].value), "[]");
            assert_eq!(obj.fields[1].id.content, "bar\"");
            assert_eq!(format!("{}", obj.fields[1].value), r#""merp\n""#);
            assert_eq!(format!("{}", Value::Object(obj)), r#"{"foo":[], "bar\"":"merp\n"}"#);
        }
        x => panic!("{:?}", x),
    }
}
#[test]
fn test_access() {
    match get_single_stmt(r#"_ = abc[6];"#).unwrap() {
        Stmt::Assign(_, Expr::Value(Value::ArrayIndex(index))) => {
            match index.src.as_ref() {
                Expr::Value(Value::Ident(id)) => {
                    assert_eq!(id.id, "abc");
                    assert_eq!(id.span(), Span(4, 7));
                }
                x => panic!("{:?}", x),
            }
            match index.index.as_ref() {
                Expr::Value(Value::Number(num)) => {
                    assert_eq!(num.value, "6");
                    assert_eq!(num.span(), Span(8, 9));
                }
                x => panic!("{:?}", x),
            }
            assert_eq!(format!("{}", index), "(abc[6])");
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = -abc[6];"#).unwrap() {
        Stmt::Assign(_, Expr::Neg(val, span_left)) => match *val {
            Expr::Value(Value::ArrayIndex(index)) => {
                match index.src.as_ref() {
                    Expr::Value(Value::Ident(id)) => {
                        assert_eq!(id.id, "abc");
                        assert_eq!(id.span(), Span(5, 8));
                    }
                    x => panic!("{:?}", x),
                }
                match index.index.as_ref() {
                    Expr::Value(Value::Number(num)) => {
                       assert_eq!(num.value, "6");
                       assert_eq!(num.span(), Span(9, 10));
                    }
                    x => panic!("{:?}", x),
                }
                let back = Expr::Neg(Box::new(Expr::Value(Value::ArrayIndex(index))), span_left);
                assert_eq!(back.span(), Span(4, 11));
                assert_eq!(back.to_string(), "(-(abc[6]))");
            }
            x => panic!("{:?}", x),
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = -abc.fieldName;"#).unwrap() {
        Stmt::Assign(_, Expr::Neg(val, span_left)) => match *val {
            Expr::Value(Value::KeyIndex(index)) => {
                match index.src.as_ref() {
                    Expr::Value(Value::Ident(id)) => {
                        assert_eq!(id.id, "abc");
                        assert_eq!(id.span(), Span(5, 8));
                    }
                    x => panic!("{:?}", x),
                }
                match index.key.as_ref() {
                    Expr::Value(Value::Text(txt)) => {
                        assert_eq!(txt.content, "fieldName");
                        assert_eq!(txt.span(), Span(9, 18));
                    }
                    x => panic!("{:?}", x),
                }
                assert_eq!(format!("{}", Expr::Neg(Box::new(Expr::Value(Value::KeyIndex(index))), span_left)), "(-(abc.[\"fieldName\"]))");
            }
            x => panic!("{:?}", x),
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = -abc.["fieldName"];"#).unwrap() {
        Stmt::Assign(_, Expr::Neg(val, span_left)) => match *val {
            Expr::Value(Value::KeyIndex(index)) => {
                match index.src.as_ref() {
                    Expr::Value(Value::Ident(id)) => {
                        assert_eq!(id.id, "abc");
                        assert_eq!(id.span(), Span(5, 8));
                    }
                    x => panic!("{:?}", x),
                }
                match index.key.as_ref() {
                    Expr::Value(Value::Text(txt)) => {
                        assert_eq!(txt.content, "fieldName");
                        assert_eq!(txt.span(), Span(10, 21));
                    }
                    x => panic!("{:?}", x),
                }
                assert_eq!(format!("{}", Expr::Neg(Box::new(Expr::Value(Value::KeyIndex(index))), span_left)), "(-(abc.[\"fieldName\"]))");
            }
            x => panic!("{:?}", x),
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = -abc.[fieldName];"#).unwrap() {
        Stmt::Assign(_, Expr::Neg(val, span_left)) => match *val {
            Expr::Value(Value::KeyIndex(index)) => {
                match index.src.as_ref() {
                    Expr::Value(Value::Ident(id)) => {
                        assert_eq!(id.id, "abc");
                        assert_eq!(id.span(), Span(5, 8));
                    }
                    x => panic!("{:?}", x),
                }
                match index.key.as_ref() {
                    Expr::Value(Value::Ident(id)) => {
                        assert_eq!(id.id, "fieldName");
                        assert_eq!(id.span(), Span(10, 19));
                    }
                    x => panic!("{:?}", x),
                }
                assert_eq!(format!("{}", Expr::Neg(Box::new(Expr::Value(Value::KeyIndex(index))), span_left)), "(-(abc.[fieldName]))");
            }
            x => panic!("{:?}", x),
        }
        x => panic!("{:?}", x),
    }

    match get_single_stmt(r#"_ = -res.a.b.c;"#).unwrap() {
        Stmt::Assign(_, val) => assert_eq!(format!("{}", val), "(-(((res.[\"a\"]).[\"b\"]).[\"c\"]))"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = !res.a[-17].c;"#).unwrap() {
        Stmt::Assign(_, val) => assert_eq!(format!("{}", val), "(!(((res.[\"a\"])[(-17)]).[\"c\"]))"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = !res.a.[v].c;"#).unwrap() {
        Stmt::Assign(_, val) => assert_eq!(format!("{}", val), "(!(((res.[\"a\"]).[v]).[\"c\"]))"),
        x => panic!("{:?}", x),
    }
}

#[test]
fn test_var_decl_assign() {
    match get_single_stmt(r#"let foo = {};"#).unwrap() {
        Stmt::VarDecl(id, val) => {
            assert_eq!(id.id, "foo");
            match val {
                Expr::Value(Value::Object(obj)) => {
                    assert_eq!(obj.fields.len(), 0);
                    assert_eq!(obj.span(), Span(10, 12));
                    assert_eq!(obj.to_string(), "{}");
                }
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"letfoo = {};"#).unwrap() {
        Stmt::Assign(lvalue, val) => {
            match lvalue {
                LValue::Ident(id) => {
                    assert_eq!(id.id, "letfoo");
                    assert_eq!(id.span(), Span(0, 6));
                }
                x => panic!("{:?}", x),
            }
            match val {
                Expr::Value(Value::Object(x)) => {
                    assert_eq!(x.fields.len(), 0);
                    assert_eq!(x.span(), Span(9, 11));
                }
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"  letfoo  [  7    ]    ={   }   ;"#).unwrap() {
        Stmt::Assign(lvalue, val) => {
            match lvalue {
                LValue::ArrayIndex(index) => {
                    match index.src.as_ref() {
                        Expr::Value(Value::Ident(id)) => {
                            assert_eq!(id.id, "letfoo");
                            assert_eq!(id.span(), Span(2, 8));
                        }
                        x => panic!("{:?}", x),
                    }
                    match index.index.as_ref() {
                        Expr::Value(Value::Number(num)) => {
                            assert_eq!(num.value, "7");
                            assert_eq!(num.span(), Span(13, 14));
                        }
                        x => panic!("{:?}", x),
                    }
                    assert_eq!(index.span(), Span(2, 19));
                }
                x => panic!("{:?}", x),
            }
            match val {
                Expr::Value(Value::Object(x)) => {
                    assert_eq!(x.fields.len(), 0);
                    assert_eq!(x.span(), Span(24, 29));
                }
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"  letfoo . baz    ={   }   ;"#).unwrap() {
        Stmt::Assign(lvalue, val) => {
            match lvalue {
                LValue::KeyIndex(index) => {
                    match index.src.as_ref() {
                        Expr::Value(Value::Ident(id)) => {
                            assert_eq!(id.id, "letfoo");
                            assert_eq!(id.span(), Span(2, 8));
                        }
                        x => panic!("{:?}", x),
                    }
                    match index.key.as_ref() {
                        Expr::Value(Value::Text(txt)) => {
                            assert_eq!(txt.content, "baz");
                            assert_eq!(txt.span(), Span(11, 14));
                        }
                        x => panic!("{:?}", x),
                    }
                    assert_eq!(index.span(), Span(2, 14));
                }
                x => panic!("{:?}", x),
            }
            match val {
                Expr::Value(Value::Object(x)) => {
                    assert_eq!(x.fields.len(), 0);
                    assert_eq!(x.span(), Span(19, 24));
                }
                x => panic!("{:?}", x),
            }
        }
        x => panic!("{:?}", x),
    }
}
#[test]
fn test_op_assoc() {
    match get_single_stmt(r#"_ = 1 == 17 ? 2 + -3^3 : a[6];"#).unwrap() {
        Stmt::Assign(_, val) => assert_eq!(format!("{}", val), "((1 == 17) ? (2 + (-(3 ^ 3))) : (a[6]))"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = 1 ? 2 ? 3 : 4 : 5;"#).unwrap() {
        Stmt::Assign(_, val) => assert_eq!(format!("{}", val), "(1 ? (2 ? 3 : 4) : 5)"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = 1 ? 2 : 3 ? 4 : 5;"#).unwrap() {
        Stmt::Assign(_, val) => assert_eq!(format!("{}", val), "(1 ? 2 : (3 ? 4 : 5))"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = 1 < 2 == 3 < 4;"#).unwrap() {
        Stmt::Assign(_, val) => assert_eq!(format!("{}", val), "((1 < 2) == (3 < 4))"),
        x => panic!("{:?}", x),
    }
    match get_single_stmt(r#"_ = a || b && c || d;"#).unwrap() {
        Stmt::Assign(_, val) => assert_eq!(format!("{}", val), "((a || (b && c)) || d)"),
        x => panic!("{:?}", x),
    }
}
