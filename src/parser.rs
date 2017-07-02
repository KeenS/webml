use nom::*;
use prim::*;
use ast::*;

named!(top <&str, AST >, do_parse!(
    opt!(multispace) >>
        tops: separated_list!(multispace, bind) >>
        opt!(complete!(multispace)) >>
        eof!() >>
        (AST(tops))
));

named!(bind <&str, Val>, alt_complete!(bind_val | bind_fun));


named!(bind_val <&str, Val>, do_parse!(
    tag_s!("val") >>
        multispace >>
        name: symbol >>
        opt!(multispace) >>
        tag_s!("=") >>
        opt!(multispace) >>
        e: expr >>
        (Val{ty: TyDefer::empty(), rec: false, name: name, expr: e})
));

named!(bind_fun <&str, Val>, do_parse!(
    tag_s!("fun") >> multispace >>
        name: symbol >> multispace >>
        params: separated_nonempty_list!(multispace, symbol) >>
        opt!(multispace) >>
        tag_s!("=") >>
        opt!(multispace) >>
        e: expr >>
        ({
            let expr =  params.into_iter().rev().fold(
                e, |acc, param|
                Expr::Fun{
                    param_ty: TyDefer::empty(),
                    param: param,
                    body_ty: TyDefer::empty(),
                    body: Box::new(acc)
                }
            );
            Val{ty: TyDefer::empty(), rec: true, name: name, expr: expr}
        })
));

named!(expr <&str, Expr>, alt_complete!(
    expr_bind |
    expr_fun  |
    expr_if   |
    expr_add  |
    expr1
));

named!(expr1 <&str, Expr>, alt_complete!(
    expr1_app |
    expr1_mul |
    expr0

));

named!(expr0 <&str, Expr>, alt_complete!(
    expr0_tuple |
    expr0_paren |
    expr0_float |
    expr0_int   |
    expr0_bool  |
    expr0_sym
));


named!(expr_bind <&str, Expr>, do_parse!(
    tag_s!("let") >> multispace >>
        binds: separated_list!(multispace, bind) >> multispace >>
        tag_s!("in") >> multispace >>
        ret: expr >> multispace >> tag_s!("end") >>
        (Expr::Binds {ty: TyDefer::empty(), binds: binds, ret: Box::new(ret)})
));

named!(expr_fun <&str, Expr>, do_parse!(
    tag!("fn") >>
        multispace >>
        param: symbol >>
        opt!(multispace) >>
        tag_s!("=>") >>
        opt!(multispace) >>
        body: expr >>
        (Expr::Fun {
            param_ty: TyDefer::empty(),
            param: param,
            body_ty: TyDefer::empty(),
            body: Box::new(body)
        })
));

named!(expr_if <&str, Expr>, do_parse!(
    tag_s!("if") >> multispace >> cond: expr >> multispace >>
        tag_s!("then") >> multispace >> then: expr >> multispace >>
        tag_s!("else") >> multispace >> else_: expr >>
        (Expr::If {
            ty: TyDefer::empty(),
            cond: Box::new(cond),
            then: Box::new(then),
            else_: Box::new(else_)
        })
));

named!(expr1_app <&str, Expr>, do_parse!(
    fun: expr0 >> multispace >>
        args: separated_nonempty_list!(multispace, expr0) >>
        ({
            let mut rest = args.into_iter();
            let arg = rest.next().unwrap();
            let init = Expr::App {ty: TyDefer::empty(), fun: Box::new(fun), arg: Box::new(arg)};
            rest.into_iter()
                .fold(init, |acc, elm| Expr::App {
                    ty: TyDefer::empty(),
                    fun: Box::new(acc),
                    arg: Box::new(elm)
                })
          })
));

named!(expr_add <&str, Expr>, do_parse!(
    e1: expr1 >>
        opt!(multispace) >>
        tag_s!("+") >>
        opt!(multispace) >>
        e2: expr >>
        (Expr::Add {ty: TyDefer::empty(), l: Box::new(e1), r: Box::new(e2)})
));

named!(expr1_mul <&str, Expr>, do_parse!(
    e1: expr0 >>
        opt!(multispace) >>
        tag_s!("*") >>
        opt!(multispace) >>
        e2: expr1 >>
        (Expr::Mul {ty: TyDefer::empty(), l: Box::new(e1), r: Box::new(e2)})
));

named!(expr0_sym <&str, Expr>, map!(symbol, |s| Expr::Sym{
    ty: TyDefer::empty(),
    name: s
}));

named!(expr0_int <&str, Expr>, map!(digit, |s: &str| Expr::Lit{
    ty: TyDefer::empty(),
    value: Literal::Int(s.parse().unwrap())}));

named!(expr0_float <&str, Expr>, map!(double_s, |s| Expr::Lit{
    ty: TyDefer::empty(),
    value: Literal::Float(s)}));


named!(expr0_bool <&str, Expr>, alt!(
    map!(tag!("true"),  |_| Expr::Lit{ty: TyDefer::empty(), value: Literal::Bool(true)}) |
    map!(tag!("false"), |_| Expr::Lit{ty: TyDefer::empty(), value: Literal::Bool(false)})));

named!(expr0_paren <&str, Expr>, do_parse!(
    tag!("(") >>
         opt!(multispace) >>
         e: expr >>
         opt!(multispace) >>
         tag!(")") >>
    (e))
);

named!(expr0_tuple <&str, Expr>, do_parse!(
    tag!("(") >>
        opt!(multispace) >>
        es: many1!(do_parse!(e: expr >> opt!(multispace) >> tag!(",") >> opt!(multispace) >> (e))) >>
        e: expr >>  opt!(multispace) >>
        tag!(")") >>
        (
            {
                let mut es = es;
                es.push(e);
                Expr::Tuple{ty: TyDefer::empty(), tuple: es}
            }
        ))
);

named!(symbol <&str, Symbol>, do_parse!(
    map_res!(peek!(alphanumeric),
             |s| match s as &str {
                 "val" | "fun" | "fn" | "let" | "in" | "end" | "if" | "then" | "else" => {
                     Err(ErrorKind::IsNot) as  ::std::result::Result<&str, ErrorKind>
                 },
                 s => Ok(s)
             }) >>
        sym: alphanumeric >> (Symbol(sym.to_string()))));

pub fn parse(input: &str) -> ::std::result::Result<AST, Err<&str>> {
    let iresult = top(input);
    iresult.to_result()
}
