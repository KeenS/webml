use crate::ast::*;
use crate::prim::*;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, digit1, multispace0, multispace1};
use nom::combinator::{all_consuming, complete, map, map_res, opt, recognize, value, verify};
use nom::multi::{many1, separated_list, separated_nonempty_list};
use nom::number::complete::recognize_float;
use nom::sequence::{preceded, terminated, tuple};
use nom::IResult;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};

static KEYWORDS: &[&str] = &[
    "val", "fun", "fn", "let", "in", "end", "if", "then", "else", "case", "of", "_", "datatype",
    "op", "=>", "infix", "infixr",
];

static RESERVED: &[&str] = &["|", "=", "#"];

struct Parser {
    infixes: RefCell<Vec<BTreeMap<u8, Vec<Symbol>>>>,
}

impl Parser {
    fn new() -> Self {
        Self {
            infixes: RefCell::new(vec![BTreeMap::new()]),
        }
    }

    fn with_scope<R>(&self, f: impl FnOnce() -> R) -> R {
        self.infixes.borrow_mut().push(BTreeMap::default());
        let r = f();
        self.infixes.borrow_mut().pop();
        r
    }

    // TODO: support let scopes
    fn new_infix_op(&self, priority: Option<u8>, mut names: Vec<Symbol>) {
        let priority = priority.unwrap_or(0);
        let mut infixes = self.infixes.borrow_mut();
        let len = infixes.len();
        infixes[len - 1]
            .entry(priority)
            .or_insert(Vec::new())
            .append(&mut names)
    }

    fn get_table(&self) -> BTreeMap<u8, Vec<Symbol>> {
        self.infixes
            .borrow()
            .iter()
            .fold(HashMap::new(), |mut acc, map| {
                for (&priority, names) in map {
                    for name in names {
                        acc.insert(name, priority);
                    }
                }
                acc
            })
            .into_iter()
            .fold(BTreeMap::new(), |mut acc, (name, priority)| {
                acc.entry(priority).or_insert(Vec::new()).push(name.clone());
                acc
            })
    }
}

impl Parser {
    fn top(&self) -> impl Fn(&str) -> IResult<&str, UntypedAst> + '_ {
        move |i| {
            let (i, _) = multispace0(i)?;
            let (i, tops) = separated_list(multispace1, self.decl())(i)?;
            let (i, _) = multispace0(i)?;
            Ok((i, AST(tops)))
        }
    }
    fn decl(&self) -> impl Fn(&str) -> IResult<&str, Declaration<()>> + '_ {
        move |i| {
            alt((
                self.decl_datatype(),
                self.decl_val(),
                self.decl_fun(),
                self.decl_infix(),
            ))(i)
        }
    }

    fn decl_datatype(&self) -> impl Fn(&str) -> IResult<&str, Declaration<()>> + '_ {
        move |i| {
            let (i, _) = tag("datatype")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, name) = self.symbol()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("=")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, constructors) = separated_nonempty_list(
                tuple((multispace0, tag("|"), multispace0)),
                self.constructor_def(),
            )(i)?;
            Ok((i, Declaration::Datatype { name, constructors }))
        }
    }

    fn decl_val(&self) -> impl Fn(&str) -> IResult<&str, Declaration<()>> + '_ {
        move |i| {
            let (i, _) = tag("val")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, pattern) = self.pattern()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("=")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, expr) = self.expr()(i)?;
            Ok((
                i,
                Declaration::Val {
                    rec: false,
                    pattern,
                    expr,
                },
            ))
        }
    }

    fn decl_fun(&self) -> impl Fn(&str) -> IResult<&str, Declaration<()>> + '_ {
        move |i| {
            let (i, _) = tag("fun")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, cs) = separated_nonempty_list(
                tuple((multispace0, tag("|"), multispace0)),
                map(
                    tuple((
                        self.decl_funbind(),
                        multispace0,
                        tag("="),
                        multispace0,
                        self.expr(),
                    )),
                    |((name, params), _, _, _, e)| (name, params, e),
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
            Ok((i, Declaration::D(DerivedDeclaration::Fun { name, clauses })))
        }
    }

    fn decl_funbind(&self) -> impl Fn(&str) -> IResult<&str, (Symbol, Vec<Pattern<()>>)> + '_ {
        move |i| {
            map(
                tuple((
                    self.op_symbol_eq(),
                    multispace0,
                    separated_nonempty_list(multispace1, self.pattern_atmic()),
                )),
                |(name, _, pats)| (name, pats),
            )(i)
        }
    }

    fn constructor_def(&self) -> impl Fn(&str) -> IResult<&str, (Symbol, Option<Type>)> + '_ {
        move |i| {
            let (i, name) = self.symbol()(i)?;
            let (i, param) = opt(complete(map(
                tuple((multispace1, tag("of"), multispace1, self.typename())),
                |(_, _, _, ty)| ty,
            )))(i)?;

            Ok((i, (name, param)))
        }
    }

    fn decl_infix(&self) -> impl Fn(&str) -> IResult<&str, Declaration<()>> + '_ {
        move |i| {
            let (i, _) = tag("infix")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, priority) = opt(digit1)(i)?;
            let (i, _) = multispace1(i)?;
            let (i, names) = separated_nonempty_list(multispace1, self.symbol_eq())(i)?;
            let priority = priority.map(|s| {
                s.parse()
                    .expect("internal error: falied to parse digits as integer")
            });
            self.new_infix_op(priority, names.clone());
            Ok((
                i,
                Declaration::D(DerivedDeclaration::Infix { priority, names }),
            ))
        }
    }

    fn expr(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            alt((
                self.expr_bind(),
                self.expr_fun(),
                self.expr_if(),
                self.expr_case(),
                self.expr_infix_and_app(),
            ))(i)
        }
    }

    fn expr1(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            alt((
                self.expr1_tuple(),
                self.expr1_unit(),
                self.expr1_paren(),
                self.expr1_float(),
                self.expr1_int(),
                self.expr1_char(),
                self.expr1_bool(),
                self.expr1_sym(),
                self.expr1_builtincall(),
                self.expr1_externcall(),
            ))(i)
        }
    }

    fn expr_bind(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            self.with_scope(|| {
                let (i, _) = tag("let")(i)?;
                let (i, _) = multispace1(i)?;
                let (i, binds) = separated_list(multispace1, self.decl())(i)?;
                let (i, _) = multispace1(i)?;
                let (i, _) = tag("in")(i)?;
                let (i, _) = multispace1(i)?;
                let (i, ret) = self.expr()(i)?;
                let (i, _) = multispace1(i)?;
                let (i, _) = tag("end")(i)?;
                Ok((
                    i,
                    Expr {
                        ty: (),
                        inner: ExprKind::Binds {
                            binds: binds,
                            ret: ret.boxed(),
                        },
                    },
                ))
            })
        }
    }

    fn expr_fun(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            let (i, _) = tag("fn")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, param) = self.symbol()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("=>")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, body) = self.expr()(i)?;
            Ok((
                i,
                Expr {
                    ty: (),
                    inner: ExprKind::Fn {
                        param: param,
                        body: body.boxed(),
                    },
                },
            ))
        }
    }

    fn expr_if(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            let (i, _) = tag("if")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, cond) = self.expr()(i)?;
            let (i, _) = multispace1(i)?;
            let (i, _) = tag("then")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, then) = self.expr()(i)?;
            let (i, _) = multispace1(i)?;
            let (i, _) = tag("else")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, else_) = self.expr()(i)?;
            Ok((
                i,
                Expr {
                    ty: (),
                    inner: ExprKind::D(DerivedExprKind::If {
                        cond: cond.boxed(),
                        then: then.boxed(),
                        else_: else_.boxed(),
                    }),
                },
            ))
        }
    }

    fn expr_case(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            let (i, _) = tag("case")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, cond) = self.expr()(i)?;
            let (i, _) = multispace1(i)?;
            let (i, _) = tag("of")(i)?;
            let (i, _) = multispace1(i)?;
            let (i, clauses) = separated_nonempty_list(
                tuple((multispace0, tag("|"), multispace0)),
                map(
                    tuple((
                        self.pattern(),
                        multispace0,
                        tag("=>"),
                        multispace0,
                        self.expr(),
                    )),
                    |(pat, _, _, _, expr)| (pat, expr),
                ),
            )(i)?;
            Ok((
                i,
                Expr {
                    ty: (),
                    inner: ExprKind::Case {
                        cond: cond.boxed(),
                        clauses: clauses,
                    },
                },
            ))
        }
    }

    // treat all of the infix operators and applications, i.e. sequeces of expressions
    fn expr_infix_and_app(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            // TODO: support 1+1
            let (i, mixed) = many1(map(tuple((multispace0, self.expr1())), |(_, e)| e))(i)?;
            #[derive(Debug)]
            enum Mixed {
                E(Expr<()>),
                Fix(u8, Symbol),
            }
            use Mixed::*;
            // find infixes
            let mixed = mixed
                .into_iter()
                .map(|mut e| match e.inner {
                    ExprKind::Symbol { name } => {
                        for (f, table) in self.get_table() {
                            if table.contains(&name) {
                                return Fix(f, name);
                            }
                        }
                        e.inner = ExprKind::Symbol { name };
                        E(e)
                    }
                    inner => {
                        e.inner = inner;
                        E(e)
                    }
                })
                .collect::<Vec<_>>();
            // reduce applys
            let rest = map_window2(mixed, |m1, m2| match (m1, m2) {
                (E(e1), E(e2)) => (
                    E(Expr {
                        ty: (),
                        inner: ExprKind::App {
                            fun: e1.boxed(),
                            arg: e2.boxed(),
                        },
                    }),
                    None,
                ),
                (m1, m2) => (m1, Some(m2)),
            });

            // reduce infixes
            fn reduce_infixl_n(n: u8, mixed: Vec<Mixed>) -> Vec<Mixed> {
                use Mixed::*;
                map_window3(mixed, |m1, m2, m3| match (m1, m2, m3) {
                    (E(l), Fix(fixty, op), E(r)) if fixty == n => (
                        E(Expr {
                            ty: (),
                            inner: ExprKind::App {
                                fun: Expr {
                                    ty: (),
                                    inner: ExprKind::Symbol { name: op },
                                }
                                .boxed(),
                                arg: Expr {
                                    ty: (),
                                    inner: ExprKind::Tuple { tuple: vec![l, r] },
                                }
                                .boxed(),
                            },
                        }),
                        None,
                    ),
                    (m1, m2, m3) => (m1, Some((m2, m3))),
                })
            }
            let mut rest = (1u8..=9)
                .rev()
                .fold(rest, |rest, n| reduce_infixl_n(n, rest));
            assert_eq!(rest.len(), 1);
            let e = match rest.remove(0) {
                E(e) => e,
                Fix(..) => unreachable!("infix alone"),
            };
            Ok((i, e))
        }
    }
    fn expr1_sym(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            // = is allowed to be used in expression exceptionally
            map(alt((self.symbol(), map(tag("="), Symbol::new))), |name| {
                Expr {
                    ty: (),
                    inner: ExprKind::Symbol { name },
                }
            })(i)
        }
    }

    fn expr1_int(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            map(digit1, |s: &str| Expr {
                ty: (),
                inner: ExprKind::Literal {
                    value: Literal::Int(s.parse().unwrap()),
                },
            })(i)
        }
    }

    fn expr1_float(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            let not_int = verify(recognize_float, |s: &&str| s.contains('.'));

            map(not_int, |s: &str| Expr {
                ty: (),
                inner: ExprKind::Literal {
                    value: Literal::Real(s.parse().unwrap()),
                },
            })(i)
        }
    }

    fn expr1_char(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            let (i, _) = tag("#")(i)?;
            let (i, s) = self.expr1_string_literal()(i)?;
            assert_eq!(s.iter().count(), 1);
            let c = s.into_iter().next().unwrap();
            Ok((
                i,
                Expr {
                    ty: (),
                    inner: ExprKind::Literal {
                        value: Literal::Char(c),
                    },
                },
            ))
        }
    }

    fn expr1_string_literal(&self) -> impl Fn(&str) -> IResult<&str, Vec<u32>> + '_ {
        move |i| {
            let (i, _) = tag("\"")(i)?;
            let mut s = vec![];
            let mut chars = i.chars();
            while let Some(c) = chars.next() {
                if c == '"' {
                    break;
                }
                s.push(c as u32)
            }
            let i = chars.as_str();
            Ok((i, s))
        }
    }

    fn expr1_bool(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            alt((
                value(
                    Expr {
                        ty: (),
                        inner: ExprKind::Constructor {
                            name: Symbol::new("true"),
                            arg: None,
                        },
                    },
                    tag("true"),
                ),
                value(
                    Expr {
                        ty: (),
                        inner: ExprKind::Constructor {
                            name: Symbol::new("false"),
                            arg: None,
                        },
                    },
                    tag("false"),
                ),
            ))(i)
        }
    }

    fn expr1_paren(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, e) = self.expr()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((i, e))
        }
    }

    fn expr1_tuple(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = multispace0(i)?;
            let sep = tuple((multispace0, tag(","), multispace0));
            let (i, es) = many1(map(tuple((self.expr(), sep)), |(e, _)| e))(i)?;
            let (i, e) = self.expr()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag(")")(i)?;

            let mut es = es;
            es.push(e);
            Ok((
                i,
                Expr {
                    ty: (),
                    inner: ExprKind::Tuple { tuple: es },
                },
            ))
        }
    }

    fn expr1_unit(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            value(
                Expr {
                    ty: (),
                    inner: ExprKind::Tuple { tuple: vec![] },
                },
                tuple((tag("("), multispace0, tag(")"))),
            )(i)
        }
    }

    fn expr1_builtincall(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            let (i, _) = tag("_builtincall")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("\"")(i)?;
            let (i, fun) = map_res(alphanumeric1, |name| match name {
                "add" => Ok(BIF::Add),
                "sub" => Ok(BIF::Sub),
                "mul" => Ok(BIF::Mul),
                "div" => Ok(BIF::Div),
                "divf" => Ok(BIF::Divf),
                "mod" => Ok(BIF::Mod),
                "eq" => Ok(BIF::Eq),
                "neq" => Ok(BIF::Neq),
                "gt" => Ok(BIF::Gt),
                "ge" => Ok(BIF::Ge),
                "lt" => Ok(BIF::Lt),
                "le" => Ok(BIF::Le),
                _ => Err(nom::Err::Error(nom::error::ErrorKind::Tag)),
            })(i)?;
            let (i, _) = tag("\"")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, args) = separated_nonempty_list(
                tuple((multispace0, tag(","), multispace0)),
                self.expr(),
            )(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((
                i,
                Expr {
                    ty: (),
                    inner: ExprKind::BuiltinCall { fun, args },
                },
            ))
        }
    }

    /// `_externcall ("module"."fun": (arg, ty) -> retty) (arg, s)`
    fn expr1_externcall(&self) -> impl Fn(&str) -> IResult<&str, Expr<()>> + '_ {
        move |i| {
            fn name_parser(i: &str) -> IResult<&str, &str> {
                let allowed = recognize(many1(nom::character::complete::none_of("\"")));
                preceded(tag("\""), terminated(allowed, tag("\"")))(i)
            }
            let (i, _) = tag("_externcall")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, module) = map(name_parser, String::from)(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag(".")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, fun) = map(name_parser, String::from)(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag(":")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, argty) = separated_nonempty_list(
                tuple((multispace0, tag(","), multispace0)),
                self.typename(),
            )(i)?;
            let (i, _) = tag(")")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("->")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, retty) = self.typename()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag(")")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, args) = separated_nonempty_list(
                tuple((multispace0, tag(","), multispace0)),
                self.expr(),
            )(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((
                i,
                Expr {
                    ty: (),
                    inner: ExprKind::ExternCall {
                        module,
                        fun,
                        args,
                        argty,
                        retty,
                    },
                },
            ))
        }
    }

    fn typename(&self) -> impl Fn(&str) -> IResult<&str, Type> + '_ {
        move |i| self.typename0()(i)
    }

    fn typename0(&self) -> impl Fn(&str) -> IResult<&str, Type> + '_ {
        move |i| alt((complete(self.typename0_fun()), self.typename1()))(i)
    }

    fn typename1(&self) -> impl Fn(&str) -> IResult<&str, Type> + '_ {
        move |i| alt((self.typename1_tuple(), self.typename2()))(i)
    }

    fn typename2(&self) -> impl Fn(&str) -> IResult<&str, Type> + '_ {
        move |i| alt((self.typename2_paren(), self.typename2_datatype()))(i)
    }

    fn typename0_fun(&self) -> impl Fn(&str) -> IResult<&str, Type> + '_ {
        move |i| {
            let (i, arg) = self.typename1()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag("->")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, ret) = self.typename()(i)?;
            Ok((i, Type::Fun(Box::new(arg), Box::new(ret))))
        }
    }

    fn typename1_tuple(&self) -> impl Fn(&str) -> IResult<&str, Type> + '_ {
        move |i| {
            let sep = tuple((multispace0, tag("*"), multispace0));

            let (i, tys) = many1(map(tuple((self.typename2(), sep)), |(ty, _)| ty))(i)?;
            let (i, ty) = self.typename2()(i)?;

            let mut tys = tys;
            tys.push(ty);
            Ok((i, Type::Tuple(tys)))
        }
    }

    fn typename2_paren(&self) -> impl Fn(&str) -> IResult<&str, Type> + '_ {
        move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, ty) = self.typename()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((i, ty))
        }
    }

    fn typename2_datatype(&self) -> impl Fn(&str) -> IResult<&str, Type> + '_ {
        move |i| {
            map(self.symbol(), |name| match name.0.as_str() {
                "unit" => Type::Tuple(vec![]),
                "real" => Type::Real,
                "int" => Type::Int,
                _ => Type::Datatype(name),
            })(i)
        }
    }

    fn symbol_eq(&self) -> impl Fn(&str) -> IResult<&str, Symbol> + '_ {
        move |i| alt((self.symbol_alphanumeric(), self.symbol_symbolic_eq()))(i)
    }

    fn symbol(&self) -> impl Fn(&str) -> IResult<&str, Symbol> + '_ {
        move |i| alt((self.symbol_alphanumeric(), self.symbol_symbolic()))(i)
    }

    fn op_symbol_eq(&self) -> impl Fn(&str) -> IResult<&str, Symbol> + '_ {
        move |i| alt((self.op_symbol_alphanumeric(), self.op_symbol_symbolic_eq()))(i)
    }

    fn op_symbol_alphanumeric(&self) -> impl Fn(&str) -> IResult<&str, Symbol> + '_ {
        move |i| {
            let (i, _) = opt(tuple((tag("op"), multispace1)))(i)?;
            self.symbol_alphanumeric()(i)
        }
    }

    fn op_symbol_symbolic_eq(&self) -> impl Fn(&str) -> IResult<&str, Symbol> + '_ {
        move |i| {
            let (i, _) = opt(tuple((tag("op"), multispace0)))(i)?;
            alt((self.symbol_symbolic(), value(Symbol::new("="), tag("="))))(i)
        }
    }

    fn symbol_alphanumeric(&self) -> impl Fn(&str) -> IResult<&str, Symbol> + '_ {
        move |i| {
            // FIXME: collect syntax is [a-zA-Z'_][a-zA-Z'_0-9]*
            let (i, sym) = verify(alphanumeric1, |s: &str| !KEYWORDS.contains(&s))(i)?;
            Ok((i, Symbol::new(sym.to_string())))
        }
    }

    fn symbol_symbolic_eq(&self) -> impl Fn(&str) -> IResult<&str, Symbol> + '_ {
        move |i| alt((self.symbol_symbolic(), value(Symbol::new("="), tag("="))))(i)
    }

    fn symbol_symbolic(&self) -> impl Fn(&str) -> IResult<&str, Symbol> + '_ {
        move |i| {
            let symbolic1 = recognize(many1(nom::character::complete::one_of(
                "!%&$#+-/:<=>?@\\~'^|*",
            )));

            let (i, sym) = verify(symbolic1, |s: &str| {
                !KEYWORDS.contains(&s) && !RESERVED.contains(&s)
            })(i)?;
            Ok((i, Symbol::new(sym.to_string())))
        }
    }

    fn pattern(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| alt((self.pattern_constructor(), self.pattern_atmic()))(i)
    }

    fn pattern_atmic(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| {
            alt((
                self.pattern_bool(),
                self.pattern_int(),
                self.pattern_tuple(),
                self.pattern_var(),
                self.pattern_wildcard(),
                self.pattern_unit(),
                self.pattern_paren(),
            ))(i)
        }
    }

    fn pattern_bool(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| {
            alt((
                map(tag("true"), |_| Pattern {
                    ty: (),
                    inner: PatternKind::Constructor {
                        name: Symbol::new("true"),
                        arg: None,
                    },
                }),
                map(tag("false"), |_| Pattern {
                    ty: (),
                    inner: PatternKind::Constructor {
                        name: Symbol::new("false"),
                        arg: None,
                    },
                }),
            ))(i)
        }
    }

    fn pattern_int(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| {
            map(digit1, |s: &str| Pattern {
                ty: (),
                inner: PatternKind::Constant {
                    value: s.parse().unwrap(),
                },
            })(i)
        }
    }

    fn pattern_tuple(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = multispace0(i)?;
            let sep = tuple((multispace0, tag(","), multispace0));
            let (i, es) = many1(map(tuple((self.pattern(), sep)), |(e, _)| e))(i)?;
            let (i, e) = self.pattern()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag(")")(i)?;

            let mut es = es;
            es.push(e);
            Ok((
                i,
                Pattern {
                    ty: (),
                    inner: PatternKind::Tuple { tuple: es },
                },
            ))
        }
    }

    fn pattern_unit(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| {
            value(
                Pattern {
                    ty: (),
                    inner: PatternKind::Tuple { tuple: vec![] },
                },
                tuple((tag("("), multispace0, tag(")"))),
            )(i)
        }
    }

    // require constructor to have arg for now.
    // constructor withouth arg is parsed as variable and
    //  will be converted in later phases
    fn pattern_constructor(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| {
            let (i, name) = self.symbol()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, arg) = self.pattern_atmic()(i)?;
            Ok((
                i,
                Pattern {
                    ty: (),
                    inner: PatternKind::Constructor {
                        name,
                        arg: Some(Box::new(arg)),
                    },
                },
            ))
        }
    }

    fn pattern_var(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| {
            map(self.symbol(), |name| Pattern {
                ty: (),
                inner: PatternKind::Variable { name: name },
            })(i)
        }
    }

    fn pattern_wildcard(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| {
            value(
                Pattern {
                    ty: (),
                    inner: PatternKind::Wildcard {},
                },
                tag("_"),
            )(i)
        }
    }

    fn pattern_paren(&self) -> impl Fn(&str) -> IResult<&str, Pattern<()>> + '_ {
        move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = multispace0(i)?;
            let (i, e) = self.pattern()(i)?;
            let (i, _) = multispace0(i)?;
            let (i, _) = tag(")")(i)?;

            Ok((i, e))
        }
    }
}

fn map_window2<I>(
    iter: I,
    mut f: impl FnMut(I::Item, I::Item) -> (I::Item, Option<I::Item>),
) -> Vec<I::Item>
where
    I: IntoIterator,
{
    let mut ret = vec![];
    let mut iter = iter.into_iter();
    let mut e = match iter.next() {
        Some(e) => e,
        None => return ret,
    };

    while let Some(e2) = iter.next() {
        let (e1, e2) = f(e, e2);
        match e2 {
            Some(e2) => {
                ret.push(e1);
                e = e2;
            }
            None => e = e1,
        }
    }
    ret.push(e);
    ret
}

fn map_window3<I>(
    iter: I,
    mut f: impl FnMut(I::Item, I::Item, I::Item) -> (I::Item, Option<(I::Item, I::Item)>),
) -> Vec<I::Item>
where
    I: IntoIterator,
{
    let mut ret = vec![];
    let mut iter = iter.into_iter();

    let mut e1 = match iter.next() {
        Some(e) => e,
        None => return ret,
    };

    let mut e2 = match iter.next() {
        Some(e) => e,
        None => {
            ret.push(e1);
            return ret;
        }
    };

    while let Some(e3) = iter.next() {
        let (r1, r2) = f(e1, e2, e3);
        match r2 {
            Some((r2, r3)) => {
                ret.push(r1);
                e1 = r2;
                e2 = r3;
            }
            None => {
                e1 = r1;
                e2 = match iter.next() {
                    Some(e) => e,
                    None => {
                        ret.push(e1);
                        return ret;
                    }
                };
            }
        }
    }
    ret.push(e1);
    ret.push(e2);
    ret
}

#[test]
fn test_expr_infix_and_app() {
    let input = "true";
    let ret = Parser::new().expr_infix_and_app()(input).unwrap();
    assert_eq!(
        ret,
        (
            "",
            Expr {
                ty: (),
                inner: ExprKind::Constructor {
                    arg: None,
                    name: Symbol::new("true")
                }
            }
        )
    )
}

#[test]
fn test_expr_infix_and_app2() {
    let input = "f arg";
    let ret = Parser::new().expr_infix_and_app()(input).unwrap();
    assert_eq!(
        ret,
        (
            "",
            Expr {
                ty: (),
                inner: ExprKind::App {
                    fun: Expr {
                        ty: (),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("f"),
                        }
                    }
                    .boxed(),
                    arg: Expr {
                        ty: (),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("arg"),
                        }
                    }
                    .boxed()
                }
            }
        )
    )
}

pub fn parse(
    input: &str,
) -> ::std::result::Result<UntypedAst, nom::Err<(&str, nom::error::ErrorKind)>> {
    let parser = Parser::new();
    let (_, iresult) = all_consuming(parser.top())(input)?;
    Ok(iresult)
}
