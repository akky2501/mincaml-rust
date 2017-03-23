use std::str;

use nom::{IResult,alpha,digit,alphanumeric,multispace};

use id::{Id, VarGenerater};
use typing::Type;
use ast::*;

#[allow(unused_variables)]
pub fn parse(src: &[u8]) -> Result<(Syntax, VarGenerater), ()> {
    match Parser::new().program(src) {
        (p,IResult::Done(inp,out)) => {
            //println!("inp:{:?} out:{:?}",inp,out);
            Ok((out,p.vg))
        },
        e@_ => {
            println!("{:?}",e);
            Err(())
        },
    }
}


macro_rules! tag_c {
    ($i:expr, $tag:expr) => (complete!($i,tag!($tag)));
}

#[derive(Debug)]
struct Parser {
    vg: VarGenerater
}

impl Parser {
    fn new() -> Self {
        Parser{ vg: VarGenerater::new() }
    }


    method!(pub program<Parser,Syntax>, mut self, complete!(
        do_parse!(
            opt!(multispace) >> e:call_m!(self.exp) >> opt!(complete!(multispace)) >> eof!() >>
            (e)
        )
    ));

    method!(exp<Parser,Syntax>, mut self, alt!(
        call_m!(self.let_exp) | call_m!(self.semicolon_exp)
    ));

    method!(let_exp<Parser,Syntax>, mut self, alt!(
          do_parse!(
              tag_c!("let") >> multispace >> 
              id:call_m!(self.ident)    >> opt!(multispace) >> 
              tag_c!("=")   >> opt!(multispace) >> 
              v:call_m!(self.exp)       >> multispace >> 
              tag_c!("in")  >> multispace >> 
              e:call_m!(self.exp) >>
              (Syntax::Let((id, self.vg.gen_type()), Box::new(v), Box::new(e)))
          )
        | do_parse!(
              tag_c!("let") >> multispace >> 
              tag_c!("rec") >> multispace >> 
              func:call_m!(self.fundef) >> multispace >> 
              tag_c!("in")  >> multispace >> 
              e:call_m!(self.exp) >>
              (Syntax::LetRec(func, Box::new(e)))
          )
        | do_parse!(
              tag_c!("let") >> multispace >> 
              tag_c!("(")   >> opt!(multispace) >> ids:call_m!(self.pat) >> opt!(multispace) >> tag_c!(")") >> opt!(multispace) >>
              tag_c!("=")   >> opt!(multispace) >> 
              v:call_m!(self.exp)       >> multispace >>
              tag_c!("in")  >> multispace >> 
              e:call_m!(self.exp) >>
              (Syntax::LetTuple(ids, Box::new(v), Box::new(e)))
          )
    ));

    method!(semicolon_exp<Parser,Syntax>, mut self, do_parse!(
        head: alt!(call_m!(self.if_exp) | call_m!(self.elems)) >>
        tail: many0!(complete!(do_parse!(
            opt!(multispace) >> tag_c!(";") >> opt!(multispace) >>
            e:alt!(call_m!(self.if_exp) | call_m!(self.elems)) >>
            (e)
        ))) >>
        
        ({
            //println!("semicolon_exp {:?} {:?}",head, tail);
            let mut tail = tail;
            if tail.is_empty() {
                head
            }
            else {
                let mut x = tail.pop().unwrap();
                while ! tail.is_empty() {
                    if let Some(w) = tail.pop() {
                        x = Syntax::Let((self.vg.gen_id(),Type::Unit), Box::new(w), Box::new(x));
                    }
                }
                Syntax::Let((self.vg.gen_id(),Type::Unit), Box::new(head), Box::new(x))
            }
        })
    ));

    method!(if_exp<Parser,Syntax>, mut self, do_parse!(
        tag_c!("if")   >> multispace >> c:alt!(call_m!(self.let_exp) | call_m!(self.elems)) >> multispace >> 
        tag_c!("then") >> multispace >> t:alt!(call_m!(self.let_exp) | call_m!(self.elems)) >> multispace >>
        tag_c!("else") >> multispace >> e:alt!(call_m!(self.let_exp) | call_m!(self.elems)) >>
        (Syntax::If(Box::new(c), Box::new(t), Box::new(e)))
    ));

    method!(elems<Parser,Syntax>, mut self, do_parse!(
        head: call_m!(self.logic_exp) >> 
        tuple:fold_many0!(complete!(do_parse!(
            opt!(multispace) >> tag_c!(",") >> opt!(multispace) >> 
            e:call_m!(self.logic_exp) >>
            (e)
        )), vec![head], |mut acc:Vec<_>, item| {acc.push(item); acc}) >>
        
        ({
            //println!("elems {:?}",tuple);
            let mut tuple = tuple;
            if tuple.len() == 1 {
                tuple.pop().unwrap()
            }
            else {
                Syntax::Tuple(tuple.into_boxed_slice())
            }
        })
    ));

    method!(logic_exp<Parser,Syntax>, mut self, do_parse!(
        head: call_m!(self.comp_exp) >>
        expr: fold_many0!(complete!(do_parse!(
                opt!(multispace) >> op:alt!(tag_c!("&&") | tag_c!("||")) >> opt!(multispace) >>
                e:call_m!(self.comp_exp) >>
                ((op,e))
        )), head, |acc:Syntax, (op,item)| {
            match str::from_utf8(op).unwrap() {
                "&&" => Syntax::If(Box::new(acc),
                                   Box::new(Syntax::If(Box::new(item), Box::new(Syntax::Bool(true)), Box::new(Syntax::Bool(false)))),
                                   Box::new(Syntax::Bool(false))),
                "||" => Syntax::If(Box::new(acc),
                                   Box::new(Syntax::Bool(true)),
                                   Box::new(Syntax::If(Box::new(item), Box::new(Syntax::Bool(true)), Box::new(Syntax::Bool(false))))),
                _ => panic!(),
            }
        }) >>
        
        (expr)
    ));

    method!(comp_exp<Parser,Syntax>, mut self, do_parse!(
        head: call_m!(self.add_exp) >>
        expr: fold_many0!(complete!(do_parse!(
                opt!(multispace) >>
                op:alt!(tag_c!("=") | tag_c!("<>") | tag_c!("<=") | tag_c!(">=") | tag_c!("<") | tag_c!(">")) >>
                opt!(multispace) >>
                e:call_m!(self.add_exp) >>
                ((op,e))
        )), head, |acc:Syntax, (op,item)| {
            match str::from_utf8(op).unwrap() {
                "="  => Syntax::Eq(Box::new(acc),Box::new(item)),
                "<>" => Syntax::Not(Box::new(Syntax::Eq(Box::new(acc),Box::new(item)))),
                "<=" => Syntax::LE(Box::new(acc),Box::new(item)),
                ">=" => Syntax::LE(Box::new(item),Box::new(acc)),
                "<"  => Syntax::Not(Box::new(Syntax::LE(Box::new(item),Box::new(acc)))),
                ">"  => Syntax::Not(Box::new(Syntax::LE(Box::new(acc),Box::new(item)))),
                _ => panic!(),
            }
        }) >>
        
        (expr)
    ));

    method!(add_exp<Parser,Syntax>, mut self, do_parse!(
        head: call_m!(self.mul_exp) >>
        expr: fold_many0!(complete!(do_parse!(
                opt!(multispace) >> op:alt!(tag_c!("+") | tag_c!("-")) >> opt!(multispace) >>
                e:call_m!(self.mul_exp) >>
                ((op,e))
        )), head, |acc:Syntax, (op,item)| {
            match str::from_utf8(op).unwrap() {
                "+"  => Syntax::Add(Box::new(acc),Box::new(item)),
                "-"  => Syntax::Sub(Box::new(acc),Box::new(item)),
                _ => panic!(),
            }
        }) >>
        
        (expr)
    ));

    method!(mul_exp<Parser,Syntax>, mut self, do_parse!(
        head: call_m!(self.unary_exp) >>
        expr: fold_many0!(complete!(do_parse!(
                opt!(multispace) >> op:alt!(tag_c!("*") | tag_c!("/")) >> opt!(multispace) >>
                e:call_m!(self.unary_exp) >>
                ((op,e))
        )), head, |acc:Syntax, (op,item)| {
            //println!("mul many0 {:?}",acc);
            match str::from_utf8(op).unwrap() {
                "*"  => Syntax::Mul(Box::new(acc),Box::new(item)),
                "/"  => Syntax::Div(Box::new(acc),Box::new(item)),
                _ => panic!(),
            }
        }) >>
        
        ({
            //println!("mul {:?}",expr);
            expr
        })
    ));

    method!(unary_exp<Parser,Syntax>, mut self, do_parse!(
        op: many0!(alt!(
                  do_parse!(s:tag_c!("-")   >> opt!(multispace) >> (str::from_utf8(s).unwrap()))
                | do_parse!(s:tag_c!("not") >> multispace >> (str::from_utf8(s).unwrap())))) >>
        app: call_m!(self.apply) >>
        ({
            //println!("unary {:?}",app);
            let mut x = app;
            let mut op = op;
            while ! op.is_empty() {
                x = match op.pop().unwrap() {
                    "-" => Syntax::Neg(Box::new(x)),
                    "not" => Syntax::Not(Box::new(x)),
                    _ => panic!(),
                }
            }

            x
        })
    ));
    
    method!(apply<Parser,Syntax>, mut self, do_parse!(
        head: call_m!(self.term) >>
        args:  many0!(complete!(do_parse!(tag_c!(" ") >> e:call_m!(self.term) >> (e)))) >>
        ({
            //println!("apply {:?} {:?}",head, args);
            if args.is_empty() {
                head
            }
            else {
                Syntax::App(Box::new(head), args.into_boxed_slice())
            }
        })
    ));
    
    method!(term<Parser,Syntax>, mut self, alt!(
          call_m!(self.lit_int)
        | call_m!(self.lit_bool)
        | call_m!(self.ident) => {|x| Syntax::Var(x)}
        | call_m!(self.lit_unit)
        | do_parse!(tag_c!("(") >> opt!(multispace) >>e:call_m!(self.exp) >> opt!(multispace) >> tag_c!(")") >> (e))
    ));
    
    method!(fundef<Parser,FunDef>, mut self, do_parse!(
        name: call_m!(self.ident) >>
        args: many1!(complete!(do_parse!(
                tag_c!(" ") >> id:call_m!(self.ident) >>
                (id)
              ))) >> opt!(multispace) >>
        tag_c!("=") >> opt!(multispace) >>
        body: call_m!(self.exp) >>
        (
            FunDef{
                name: (name, self.vg.gen_type()), 
                args: args.into_iter()
                          .map(|x| (x,self.vg.gen_type()))
                          .collect::<Vec<_>>().into_boxed_slice(), 
                body: Box::new(body)
            }
        )
    ));
    
    method!(pat<Parser,Box<[(Id,Type)]>>, mut self, do_parse!(
        head: call_m!(self.ident) >>
        list: fold_many0!(complete!(do_parse!(
                opt!(multispace) >> tag_c!(",") >> opt!(multispace) >> id:call_m!(self.ident) >>
                (id)
            )),vec![head], |mut acc:Vec<_>, item|{acc.push(item); acc}) >>
        ({
            list.into_iter()
                .map(|x| (x, self.vg.gen_type()))
                .collect::<Vec<(Id,Type)>>()
                .into_boxed_slice()
        })
    ));
   
    method!(ident<Parser,Id>, mut self, do_parse!(
        not!(
            do_parse!(r:call_m!(self.reserved) >> not!(alt!(tag_c!("_")|alphanumeric)) >> (r))
        ) >>
        head: alt!(tag_c!("_")|alpha) >>
        string: fold_many0!(complete!(alt!(tag_c!("_") | alphanumeric)),
                            str::from_utf8(head).unwrap().to_string(),
                            |mut acc:String, item|{ acc.push_str(str::from_utf8(item).unwrap()); acc }) >>
        (Id(string))
    ));

    method!(reserved<Parser>, self, alt!(
        tag_c!("let") | tag_c!("in")   | tag_c!("rec")  | 
        tag_c!("if")  | tag_c!("then") | tag_c!("else") |
        tag_c!("not") | tag_c!("true") | tag_c!("false")
    ));
    
    method!(lit_int<Parser,Syntax>, self, do_parse!(
        n:alt!(tag_c!("0")|digit) >>
        (Syntax::Int(
            i64::from_str_radix(
                str::from_utf8(n).unwrap(),
                10
            ).unwrap()))
    ));
    
    method!(lit_bool<Parser,Syntax>, self, alt!(
          tag_c!("true")  => {|_| Syntax::Bool(true) }
        | tag_c!("false") => {|_| Syntax::Bool(false)}
    ));

    method!(lit_unit<Parser,Syntax>, self, do_parse!(
        tag_c!("()") >> (Syntax::Unit)
    ));
}





/* mincaml peg

~ <- muitispace(\n\r\t' ')

program <- ~? exp ~? $

exp <- (let_exp / semicolon_exp)

let_exp <- "let" ~ ident ~? "=" ~? exp ~ "in" ~ exp
         / "let" ~ "rec" ~ fundef ~ "in" ~ exp
         / "let" ~ "(" ~? pat ~? ")" ~? "=" ~? exp ~ "in" ~ exp

semicolon_exp <- (if_exp / elems) ( ~? ";" ~? (if_exp / elems))*

if_exp <- "if" ~ exp ~ "then" ~ exp ~ "else" ~ exp

elems <- logic_exp ( ~? "," ~? logic_exp)*

logic_exp <- comp_exp ( ~? ("&&" / "||") ~? comp_exp)*

comp_exp <- add_exp ( ~? ("=" / "<>" / "<=" / ">=" / "<" / ">") ~? add_exp)*

add_exp <- mul_exp ( ~? ("+" / "-") ~? mul_exp)*

mul_exp <- unary_exp ( ~? ("*" / "/") ~? unary_exp)*

unary_exp <- ("-" ~? / "not" ~)* apply

apply <- term (" " term)*

term <- ident
      / lit_int
      / lit_bool
      / lit_unit
      / "(" ~? exp ~? ")"

fundef <- ident ( " " ident)+ ~? "=" ~? exp

pat <- ident ( ~? "," ~? ident)*

ident <- reserved! [_a-zA-Z][_a-zA-Z0-9]*
reserved <- "let" / "in" / "rec" / "if" / "then" / "else" / "not" / "and" / "or" / "true" / "false"

lit_int <- "0" / [1-9][0-9]*

lit_bool <- "true" / "false"

lit_unit <- "()"

*/
