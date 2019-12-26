use crate::ast::*;
use crate::prim::*;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, digit1, multispace0, multispace1};
use nom::combinator::{all_consuming, complete, map, map_res, opt, peek, recognize, value, verify};
use nom::multi::{many1, separated_list, separated_nonempty_list};
use nom::number::complete::recognize_float;
use nom::sequence::{preceded, tuple};
use nom::IResult;

static KEYWORDS: &[&str] = &[
    "val", "fun", "fn", "let", "in", "end", "if", "then", "else", "case", "of", "_", "datatype",
];
static INFIX7: &[&str] = &["*", "/", "div", "mod"];
static INFIX6: &[&str] = &["+", "-"];
static INFIX5: &[&str] = &[];
static INFIX4: &[&str] = &["=", "<>", "<=", "<", ">=", ">"];
static INFIX3: &[&str] = &[];
static INFIX2: &[&str] = &[];
static INFIX1: &[&str] = &[];

fn one_of<'b>(
    tags: &'b [&str],
) -> impl for<'a> Fn(&'a str) -> IResult<&'a str, &'a str> + Clone + 'b {
    move |i: &str| -> IResult<&str, &str> {
        for tag in tags {
            if i.starts_with(tag) {
                return Ok((&i[(tag.len())..], &i[0..(tag.len())]));
            }
        }
        Err(nom::Err::Error((i, nom::error::ErrorKind::IsNot)))
    }
}

fn top(i: &str) -> IResult<&str, UntypedAst> {
    let (i, _) = multispace0(i)?;
    let (i, tops) = separated_list(multispace1, bind)(i)?;
    let (i, _) = multispace0(i)?;
    Ok((i, AST(tops)))
}

fn bind(i: &str) -> IResult<&str, Statement<()>> {
    alt((bind_datatype, bind_val, bind_fun))(i)
}

fn bind_datatype(i: &str) -> IResult<&str, Statement<()>> {
    let (i, _) = tag("datatype")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, name) = symbol(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, constructors) =
        separated_nonempty_list(tuple((multispace0, tag("|"), multispace0)), constructor_def)(i)?;
    Ok((i, Statement::Datatype { name, constructors }))
}

fn bind_val(i: &str) -> IResult<&str, Statement<()>> {
    let (i, _) = tag("val")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, pattern) = pattern_single(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("=")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, expr) = expr(i)?;
    Ok((
        i,
        Statement::Val {
            rec: false,
            pattern,
            expr,
        },
    ))
}

fn bind_fun(i: &str) -> IResult<&str, Statement<()>> {
    let (i, _) = tag("fun")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, cs) = separated_nonempty_list(
        tuple((multispace0, tag("|"), multispace0)),
        map(
            tuple((
                symbol,
                multispace0,
                separated_nonempty_list(multispace1, pattern_multi),
                multispace0,
                tag("="),
                multispace0,
                expr,
            )),
            |(name, _, params, _, _, _, e)| (name, params, e),
        ),
    )(i)?;
    let mut cs = cs.into_iter();
    let (name, params, expr) = cs.next().expect("nonempty list empty");
    let mut clauses = vec![(params, expr)];
    for (new_name, params, expr) in cs {
        if name != new_name {
            return Err(nom::Err::Error((i, nom::error::ErrorKind::Tag)));
        }
        clauses.push((params, expr))
    }
    Ok((i, Statement::D(DerivedStatement::Fun { name, clauses })))
}

fn constructor_def(i: &str) -> IResult<&str, (Symbol, Option<Type>)> {
    let (i, name) = symbol(i)?;
    let (i, param) = opt(complete(map(
        tuple((multispace1, tag("of"), multispace1, typename)),
        |(_, _, _, ty)| ty,
    )))(i)?;

    Ok((i, (name, param)))
}
fn expr(i: &str) -> IResult<&str, Expr<()>> {
    alt((expr_bind, expr_fun, expr_if, expr_case, infix4))(i)
}

// infix 4
fn infix4(i: &str) -> IResult<&str, Expr<()>> {
    alt((infix4_op, infix5))(i)
}

// infix 5
fn infix5(i: &str) -> IResult<&str, Expr<()>> {
    alt((infix5_op, infix6))(i)
}

// infix 6
fn infix6(i: &str) -> IResult<&str, Expr<()>> {
    alt((infix6_op, infix7))(i)
}

// infix 7
fn infix7(i: &str) -> IResult<&str, Expr<()>> {
    alt((infix7_op, expr0))(i)
}

fn expr0(i: &str) -> IResult<&str, Expr<()>> {
    alt((expr0_app, expr1))(i)
}

fn expr1(i: &str) -> IResult<&str, Expr<()>> {
    alt((
        expr1_tuple,
        expr1_paren,
        expr1_float,
        expr1_int,
        expr1_bool,
        expr1_sym,
        expr1_builtincall,
    ))(i)
}

fn expr_bind(i: &str) -> IResult<&str, Expr<()>> {
    let (i, _) = tag("let")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, binds) = separated_list(multispace1, bind)(i)?;
    let (i, _) = multispace1(i)?;
    let (i, _) = tag("in")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, ret) = expr(i)?;
    let (i, _) = multispace1(i)?;
    let (i, _) = tag("end")(i)?;
    Ok((
        i,
        Expr::Binds {
            ty: (),
            binds: binds,
            ret: ret.boxed(),
        },
    ))
}

fn expr_fun(i: &str) -> IResult<&str, Expr<()>> {
    let (i, _) = tag("fn")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, param) = symbol(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("=>")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, body) = expr(i)?;
    Ok((
        i,
        Expr::Fn {
            ty: (),
            param: param,
            body: body.boxed(),
        },
    ))
}

fn expr_if(i: &str) -> IResult<&str, Expr<()>> {
    let (i, _) = tag("if")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, cond) = expr(i)?;
    let (i, _) = multispace1(i)?;
    let (i, _) = tag("then")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, then) = expr(i)?;
    let (i, _) = multispace1(i)?;
    let (i, _) = tag("else")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, else_) = expr(i)?;
    Ok((
        i,
        Expr::D(DerivedExpr::If {
            ty: (),
            cond: cond.boxed(),
            then: then.boxed(),
            else_: else_.boxed(),
        }),
    ))
}

fn expr_case(i: &str) -> IResult<&str, Expr<()>> {
    let (i, _) = tag("case")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, cond) = expr(i)?;
    let (i, _) = multispace1(i)?;
    let (i, _) = tag("of")(i)?;
    let (i, _) = multispace1(i)?;
    let (i, clauses) = separated_nonempty_list(
        tuple((multispace0, tag("|"), multispace0)),
        map(
            tuple((pattern_single, multispace0, tag("=>"), multispace0, expr)),
            |(pat, _, _, _, expr)| (pat, expr),
        ),
    )(i)?;
    Ok((
        i,
        Expr::Case {
            ty: (),
            cond: cond.boxed(),
            clauses: clauses,
        },
    ))
}

// left recursion eliminated
fn infixl(
    subexpr: impl Fn(&str) -> IResult<&str, Expr<()>> + Clone,
    op_parser: impl Fn(&str) -> IResult<&str, &str> + Clone,
) -> impl Fn(&str) -> IResult<&str, Expr<()>> {
    move |i: &str| -> IResult<&str, Expr<()>> {
        let (i, l) = subexpr(i)?;
        let (i, _) = multispace0(i)?;
        let (i, op) = op_parser(i)?;
        let (i, _) = multispace0(i)?;
        let (i, r) = subexpr(i)?;
        let mut e = Expr::BinOp {
            op: Symbol::new(op),
            ty: (),
            l: l.boxed(),
            r: r.boxed(),
        };
        let mut pin = i;
        loop {
            match map(
                tuple((multispace0, op_parser.clone(), multispace0, subexpr.clone())),
                |(_, op, _, r)| (op, r),
            )(pin)
            {
                Ok((i, (op, r))) => {
                    pin = i;
                    e = Expr::BinOp {
                        op: Symbol::new(op),
                        ty: (),
                        l: e.boxed(),
                        r: r.boxed(),
                    }
                }
                Err(_) => return Ok((pin, e)),
            }
        }
    }
}

fn infix7_op(i: &str) -> IResult<&str, Expr<()>> {
    infixl(expr0, one_of(INFIX7))(i)
}

fn infix6_op(i: &str) -> IResult<&str, Expr<()>> {
    infixl(infix7, one_of(INFIX6))(i)
}

fn infix5_op(i: &str) -> IResult<&str, Expr<()>> {
    infixl(infix6, one_of(INFIX5))(i)
}

fn infix4_op(i: &str) -> IResult<&str, Expr<()>> {
    infixl(infix5, one_of(INFIX4))(i)
}

fn expr0_app(i: &str) -> IResult<&str, Expr<()>> {
    // left-recursion is eliminated
    let (i, fun) = expr1(i)?;
    let (i, _) = multispace1(i)?;

    let non_op = map_res(peek(recognize(expr1)), |s| {
        if [INFIX1, INFIX2, INFIX3, INFIX4, INFIX5, INFIX6, INFIX7]
            .iter()
            .any(|tags| tags.contains(&s))
        {
            Err(nom::error::ErrorKind::IsNot)
        } else {
            Ok(s)
        }
    });

    let (i, args) = separated_nonempty_list(multispace1, preceded(non_op, expr1))(i)?;

    let mut rest = args.into_iter();
    let arg = rest.next().unwrap();
    let init = Expr::App {
        ty: (),
        fun: fun.boxed(),
        arg: arg.boxed(),
    };
    Ok((
        i,
        rest.fold(init, |acc, elm| Expr::App {
            ty: (),
            fun: acc.boxed(),
            arg: elm.boxed(),
        }),
    ))
}

fn expr1_sym(i: &str) -> IResult<&str, Expr<()>> {
    map(symbol, |s| Expr::Symbol { ty: (), name: s })(i)
}

fn expr1_int(i: &str) -> IResult<&str, Expr<()>> {
    map(digit1, |s: &str| Expr::Literal {
        ty: (),
        value: Literal::Int(s.parse().unwrap()),
    })(i)
}

fn expr1_float(i: &str) -> IResult<&str, Expr<()>> {
    let not_int = verify(recognize_float, |s: &&str| s.contains('.'));

    map(not_int, |s: &str| Expr::Literal {
        ty: (),
        value: Literal::Real(s.parse().unwrap()),
    })(i)
}

fn expr1_bool(i: &str) -> IResult<&str, Expr<()>> {
    alt((
        value(
            Expr::Constructor {
                name: Symbol::new("true"),
                arg: None,
                ty: (),
            },
            tag("true"),
        ),
        value(
            Expr::Constructor {
                name: Symbol::new("false"),
                arg: None,
                ty: (),
            },
            tag("false"),
        ),
    ))(i)
}

fn expr1_paren(i: &str) -> IResult<&str, Expr<()>> {
    let (i, _) = tag("(")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, e) = expr(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag(")")(i)?;
    Ok((i, e))
}

fn expr1_tuple(i: &str) -> IResult<&str, Expr<()>> {
    let (i, _) = tag("(")(i)?;
    let (i, _) = multispace0(i)?;
    let sep = tuple((multispace0, tag(","), multispace0));
    let (i, es) = many1(map(tuple((expr, sep)), |(e, _)| e))(i)?;
    let (i, e) = expr(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag(")")(i)?;

    let mut es = es;
    es.push(e);
    Ok((i, Expr::Tuple { ty: (), tuple: es }))
}

fn expr1_builtincall(i: &str) -> IResult<&str, Expr<()>> {
    let (i, _) = tag("_builtincall")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("\"")(i)?;
    let (i, name) = alphanumeric1(i)?;
    let (i, _) = tag("\"")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("(")(i)?;
    let (i, args) = separated_nonempty_list(tuple((multispace0, tag(","), multispace0)), expr)(i)?;
    let (i, _) = tag(")")(i)?;
    Ok((
        i,
        Expr::BuiltinCall {
            ty: (),
            name: name.to_string(),
            args,
        },
    ))
}

fn typename(i: &str) -> IResult<&str, Type> {
    typename0(i)
}

fn typename0(i: &str) -> IResult<&str, Type> {
    alt((complete(typename0_fun), typename1))(i)
}

fn typename1(i: &str) -> IResult<&str, Type> {
    alt((typename1_tuple, typename2))(i)
}

fn typename2(i: &str) -> IResult<&str, Type> {
    alt((
        typename2_int,
        typename2_real,
        typename2_unit,
        typename2_paren,
        typename2_datatype,
    ))(i)
}

fn typename0_fun(i: &str) -> IResult<&str, Type> {
    let (i, arg) = typename1(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag("->")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, ret) = typename(i)?;
    Ok((i, Type::Fun(Box::new(arg), Box::new(ret))))
}

fn typename1_tuple(i: &str) -> IResult<&str, Type> {
    let sep = tuple((multispace0, tag("*"), multispace0));

    let (i, tys) = many1(map(tuple((typename2, sep)), |(ty, _)| ty))(i)?;
    let (i, ty) = typename2(i)?;

    let mut tys = tys;
    tys.push(ty);
    Ok((i, Type::Tuple(tys)))
}

fn typename2_int(i: &str) -> IResult<&str, Type> {
    map(tag("int"), |_| Type::Int)(i)
}

fn typename2_real(i: &str) -> IResult<&str, Type> {
    map(tag("real"), |_| Type::Real)(i)
}

fn typename2_unit(i: &str) -> IResult<&str, Type> {
    map(tag("()"), |_| Type::Tuple(vec![]))(i)
}

fn typename2_paren(i: &str) -> IResult<&str, Type> {
    let (i, _) = tag("(")(i)?;
    let (i, _) = multispace0(i)?;
    let (i, ty) = typename(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag(")")(i)?;
    Ok((i, ty))
}

fn typename2_datatype(i: &str) -> IResult<&str, Type> {
    map(symbol, |name| Type::Datatype(name))(i)
}

// TODO: use verify
fn symbol(i: &str) -> IResult<&str, Symbol> {
    let (i, _) = map_res(peek(alphanumeric1), |s| {
        if KEYWORDS.contains(&s) {
            Err(nom::error::ErrorKind::IsNot)
        } else {
            Ok(s)
        }
    })(i)?;
    let (i, sym) = alphanumeric1(i)?;
    Ok((i, Symbol::new(sym.to_string())))
}

fn pattern_single(i: &str) -> IResult<&str, Pattern<()>> {
    alt((
        pattern_bool,
        pattern_int,
        pattern_tuple,
        pattern_constructor,
        pattern_var,
        pattern_wildcard,
    ))(i)
}

fn pattern_multi(i: &str) -> IResult<&str, Pattern<()>> {
    alt((
        pattern_bool,
        pattern_int,
        pattern_tuple,
        pattern_var,
        pattern_wildcard,
    ))(i)
}

fn pattern_bool(i: &str) -> IResult<&str, Pattern<()>> {
    alt((
        map(tag("true"), |_| Pattern::Constructor {
            name: Symbol::new("true"),
            arg: None,
            ty: (),
        }),
        map(tag("false"), |_| Pattern::Constructor {
            name: Symbol::new("false"),
            arg: None,
            ty: (),
        }),
    ))(i)
}

fn pattern_int(i: &str) -> IResult<&str, Pattern<()>> {
    map(digit1, |s: &str| Pattern::Constant {
        ty: (),
        value: s.parse().unwrap(),
    })(i)
}

fn pattern_tuple(i: &str) -> IResult<&str, Pattern<()>> {
    let (i, _) = tag("(")(i)?;
    let (i, _) = multispace0(i)?;
    let sep = tuple((multispace0, tag(","), multispace0));
    let (i, es) = many1(map(tuple((pattern_single, sep)), |(e, _)| e))(i)?;
    let (i, e) = pattern_single(i)?;
    let (i, _) = multispace0(i)?;
    let (i, _) = tag(")")(i)?;

    let mut es = es;
    es.push(e);
    Ok((i, Pattern::Tuple { tuple: es, ty: () }))
}

// require constructor to have arg for now.
// it will be converted in later phases
fn pattern_constructor(i: &str) -> IResult<&str, Pattern<()>> {
    let (i, name) = symbol(i)?;
    let (i, _) = multispace1(i)?;
    let (i, arg) = pattern_multi(i)?;
    Ok((
        i,
        Pattern::Constructor {
            name,
            arg: Some(Box::new(arg)),
            ty: (),
        },
    ))
}

fn pattern_var(i: &str) -> IResult<&str, Pattern<()>> {
    map(symbol, |name| Pattern::Variable { name: name, ty: () })(i)
}

fn pattern_wildcard(i: &str) -> IResult<&str, Pattern<()>> {
    map(tag("_"), |_| Pattern::Wildcard { ty: () })(i)
}

pub fn parse(
    input: &str,
) -> ::std::result::Result<UntypedAst, nom::Err<(&str, nom::error::ErrorKind)>> {
    let (_, iresult) = all_consuming(top)(input)?;
    Ok(iresult)
}
