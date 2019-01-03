use crate::ast::*;
use crate::prim::*;
use nom::*;

static KEYWORDS: &[&str] = &[
    "val", "fun", "fn", "let", "in", "end", "if", "then", "else", "case", "of", "_",
];
static INFIX7: &[&str] = &["*", "/", "div", "mod"];
static INFIX6: &[&str] = &["+", "-"];
static INFIX5: &[&str] = &[];
static INFIX4: &[&str] = &["=", "<>", "<=", "<", ">=", ">"];
static INFIX3: &[&str] = &[];
static INFIX2: &[&str] = &[];
static INFIX1: &[&str] = &[];

fn one_of<'a, 'b>(input: &'a str, tags: &'b [&str]) -> IResult<&'a str, &'a str> {
    for tag in tags {
        if input.starts_with(tag) {
            return IResult::Done(&input[(tag.len())..], &input[0..(tag.len())]);
        }
    }
    IResult::Error(Err::Code(ErrorKind::IsNotStr))
}

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
        pat: pattern >>
        opt!(multispace) >>
        tag_s!("=") >>
        opt!(multispace) >>
        e: expr >>
        (Val{ty: TyDefer::empty(), rec: false, pattern: pat, expr: e})
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
            Val{ty: TyDefer::empty(), rec: true, pattern: Pattern::Var{name: name, ty: TyDefer::empty()}, expr: expr}
        })
));

named!(expr <&str, Expr>, alt_complete!(
    expr_bind |
    expr_fun  |
    expr_if   |
    expr_case |
    infix4
));

// infix 4
named!(infix4 <&str, Expr>, alt_complete!(
    infix4_op |
    infix5

));

// infix 5
named!(infix5 <&str, Expr>, alt_complete!(
    infix5_op |
    infix6
));

// infix 6
named!(infix6 <&str, Expr>, alt_complete!(
    infix6_op |
    infix7

));

// infix 7
named!(infix7 <&str, Expr>, alt_complete!(
    infix7_op |
    expr0

));

named!(expr0 <&str, Expr>, alt_complete!(
    expr0_app |
    expr1

));

named!(expr1 <&str, Expr>, alt_complete!(
    expr1_tuple |
    expr1_paren |
    expr1_float |
    expr1_int   |
    expr1_bool  |
    expr1_sym
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

named!(expr_case <&str, Expr>, do_parse!(
    tag_s!("case") >> multispace >> cond: expr >> multispace >>
        tag_s!("of") >> multispace >>
        clauses: separated_nonempty_list!(
            do_parse!(opt!(multispace) >> tag!("|") >> opt!(multispace) >> (())),
            do_parse!(
                pat: pattern >> opt!(multispace) >>
                    tag_s!("=>") >> opt!(multispace) >>
                    expr: expr >>
                    (pat, expr))) >>
        (Expr::Case {
            ty: TyDefer::empty(),
            cond: Box::new(cond),
            clauses: clauses,
        })
));

// FIXME make left associative
named!(infix7_op <&str, Expr>, do_parse!(
    e1: expr0 >>
        opt!(multispace) >>
        op: call!(one_of, INFIX7) >>
        opt!(multispace) >>
        e2: infix7 >>
        (Expr::BinOp {
            op: Symbol::new(op),
            ty: TyDefer::empty(),
            l: Box::new(e1),
            r: Box::new(e2)
        })
));

// FIXME make left associative
named!(infix6_op <&str, Expr>, do_parse!(
    e1: infix7 >>
        opt!(multispace) >>
        op: call!(one_of, INFIX6) >>
        opt!(multispace) >>
        e2: infix6 >>
        (Expr::BinOp {
            op: Symbol::new(op),
            ty: TyDefer::empty(),
            l: Box::new(e1),
            r: Box::new(e2)
        })
));

// FIXME make left associative
named!(infix5_op <&str, Expr>, do_parse!(
    e1: infix6 >>
        opt!(multispace) >>
        op: call!(one_of, INFIX5) >>
        opt!(multispace) >>
        e2: infix5 >>
        (Expr::BinOp {
            op: Symbol::new(op),
            ty: TyDefer::empty(),
            l: Box::new(e1),
            r: Box::new(e2)
        })
));

// FIXME make left associative
named!(infix4_op <&str, Expr>, do_parse!(
    e1: infix5 >>
        opt!(multispace) >>
        op: call!(one_of, INFIX4) >>
        opt!(multispace) >>
        e2: infix4 >>
        (Expr::BinOp {
            op: Symbol::new(op),
            ty: TyDefer::empty(),
            l: Box::new(e1),
            r: Box::new(e2)
        })
));

named!(expr0_app <&str, Expr>, do_parse!(
    // left-recursion is eliminated
    fun: expr1 >> multispace >>
        args: separated_nonempty_list!(
            multispace,
            do_parse!(
                map_res!(peek!(recognize!(expr1)),
                         |s| if [
                             INFIX1, INFIX2, INFIX3,
                             INFIX4, INFIX5, INFIX6,
                             INFIX7].iter().any(|tags| tags.contains(&s)) {
                             Err(ErrorKind::IsNot) as  ::std::result::Result<&str, ErrorKind>
                         } else {
                             Ok(s)
                         }
                ) >>
                    e: expr1 >> (e)))
        >>
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

named!(expr1_sym <&str, Expr>, map!(symbol, |s| Expr::Sym{
    ty: TyDefer::empty(),
    name: s
}));

named!(expr1_int <&str, Expr>, map!(digit, |s: &str| Expr::Lit{
    ty: TyDefer::empty(),
    value: Literal::Int(s.parse().unwrap())}));

named!(expr1_float <&str, Expr>, map!(double_s, |s| Expr::Lit{
    ty: TyDefer::empty(),
    value: Literal::Float(s)}));

named!(expr1_bool <&str, Expr>, alt!(
    map!(tag!("true"),  |_| Expr::Lit{ty: TyDefer::empty(), value: Literal::Bool(true)}) |
    map!(tag!("false"), |_| Expr::Lit{ty: TyDefer::empty(), value: Literal::Bool(false)})));

named!(expr1_paren <&str, Expr>, do_parse!(
    tag!("(") >>
         opt!(multispace) >>
         e: expr >>
         opt!(multispace) >>
         tag!(")") >>
    (e))
);

named!(expr1_tuple <&str, Expr>, do_parse!(
    tag!("(") >>
        opt!(multispace) >>
        es: many1!(do_parse!(
            e: expr >> opt!(multispace)
                >> tag!(",") >> opt!(multispace) >> (e))
        ) >>
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

// TODO: use verify
named!(symbol <&str, Symbol>, do_parse!(
    map_res!(peek!(alphanumeric),
             |s| if KEYWORDS.contains(&s) {
                 Err(ErrorKind::IsNot) as  ::std::result::Result<&str, ErrorKind>
             } else {
                 Ok(s)
             }
             ) >>
        sym: alphanumeric >> (Symbol::new(sym.to_string()))));

named!(pattern <&str, Pattern>, alt_complete!(
    pattern_bool |
    pattern_int |
    pattern_tuple |
    pattern_var |
    pattern_wildcard));

named!(pattern_bool <&str, Pattern>, alt!(
    map!(tag!("true"),  |_| Pattern::Lit{value: Literal::Bool(true), ty: TyDefer::empty()}) |
    map!(tag!("false"), |_| Pattern::Lit{value: Literal::Bool(false), ty: TyDefer::empty()})));

named!(pattern_tuple <&str, Pattern>, do_parse!(
    tag!("(") >>
        opt!(multispace) >>
        es: many1!(do_parse!(
            e: symbol >> opt!(multispace)
                >> tag!(",") >> opt!(multispace) >> (e))
        ) >>
        e: symbol >>  opt!(multispace) >>
        tag!(")") >>
        (
            {
                let mut es = es;
                es.push(e);
                Pattern::Tuple{tuple: es.into_iter().map(|e| (TyDefer::empty(), e)).collect()}
            }
        ))
);

named!(pattern_var <&str, Pattern>, map!(symbol, |name| Pattern::Var {
    name: name,
    ty: TyDefer::empty()
}));

named!(pattern_wildcard <&str, Pattern>, map!(tag!("_"), |name| Pattern::Wildcard {
    ty: TyDefer::empty()
}));

named!(pattern_int <&str, Pattern>, map!(digit, |s: &str| Pattern::Lit{
    ty: TyDefer::empty(),
    value: Literal::Int(s.parse().unwrap())}));

pub fn parse(input: &str) -> ::std::result::Result<AST, Err<&str>> {
    let iresult = top(input);
    iresult.to_result()
}
