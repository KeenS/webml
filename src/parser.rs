use std::str::from_utf8;
use nom::*;
use ast::*;
use ty::*;

named!(top < Vec<AST> >, do_parse!(
    opt!(multispace) >>
        tops: separated_list!(multispace, map!(bind, AST::Top)) >>
        opt!(multispace) >>
        (tops)
));

named!(bind <Bind>, alt!(
    map!(bind_val, Bind::V)
));

named!(bind_val <Val>, do_parse!(
    tag!("val") >>
        multispace >>
        name: symbol >>
        opt!(multispace) >>
        tag!("=") >>
        opt!(multispace) >>
        e: expr >>
        (Val{ty: TyDefer::empty(), name: name, expr: e})
));

named!(expr <Expr>, alt!(
    expr_bind |
    expr_fun  |
    expr_if   |
    expr_add  |
    expr1

));

named!(expr1 <Expr>, alt!(
    expr1_mul |
    factor

));

named!(factor <Expr>, alt!(
    factor_paren |
    factor_int   |
    factor_bool  |
    factor_sym
));

named!(expr_bind <Expr>, do_parse!(
    tag!("let") >> multispace >>
        binds: separated_list!(multispace, bind) >> multispace >>
        tag!("in") >> multispace >>
        ret: expr >> multispace >> tag!("end") >>
        (Expr::Binds {ty: TyDefer::empty(), binds: binds, ret: Box::new(ret)})
));

named!(expr_fun <Expr>, do_parse!(
    tag!("fun") >>
        multispace >>
        arg: symbol >>
        opt!(multispace) >>
        tag!("=>") >>
        opt!(multispace) >>
        body: expr >>
        (Expr::Fun {ty: TyDefer::empty(), arg: arg, body: Box::new(body)})
));

named!(expr_if <Expr>, do_parse!(
    tag!("if") >> multispace >> cond: expr >> multispace >>
        tag!("then") >> multispace >> then: expr >> multispace >>
        tag!("else") >> multispace >> else_: expr >>
        (Expr::If {ty: TyDefer::empty(), cond: Box::new(cond), then: Box::new(then), else_: Box::new(else_)})
));

named!(expr_add <Expr>, do_parse!(
    e1: expr1 >>
        opt!(multispace) >>
        tag!("+") >>
        opt!(multispace) >>
        e2: expr >>
        (Expr::Add {ty: TyDefer::empty(), l: Box::new(e1), r: Box::new(e2)})
));

named!(expr1_mul <Expr>, do_parse!(
    e1: factor >>
        opt!(multispace) >>
        tag!("*") >>
        opt!(multispace) >>
        e2: expr1 >>
        (Expr::Mul {ty: TyDefer::empty(), l: Box::new(e1), r: Box::new(e2)})
));

named!(factor_sym <Expr>, map!(symbol, Expr::Sym));
named!(factor_int <Expr>, map!(digit, |s| Expr::LitInt(from_utf8(s).expect("internal error: failed to parse integer literal")
                                                 .parse().expect("internal error: failed to parse integer literal"))));
named!(factor_bool <Expr>, alt!(
    map!(tag!("true"),  |_| Expr::LitBool(true)) |
    map!(tag!("false"), |_| Expr::LitBool(false))));

named!(factor_paren <Expr>, do_parse!(
    tag!("(") >>
         opt!(multispace) >>
         e: expr >>
         opt!(multispace) >>
         tag!(")") >>
    (e))
);

named!(symbol <Symbol>, map_res!(
    alphanumeric, |s| match s as &[u8] {
        b"val" | b"fun" | b"let" | b"in" | b"end" | b"if" | b"then" | b"else" => {
            Err(ErrorKind::IsNot) as  Result<Symbol, ErrorKind>
        },
        s => Ok(Symbol(from_utf8(s).expect("failed to parse UTF-8 value").to_string()))
    }));

pub fn parse(input: &[u8]) -> Result<Vec<AST>, ErrorKind> {
    let iresult = top(input);
    iresult.to_result()
}
