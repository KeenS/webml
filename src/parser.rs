use crate::pass::Pass;
use crate::prim::*;
use crate::{ast::*, Config};
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric1, digit1, multispace1};
use nom::combinator::{all_consuming, complete, map, map_res, opt, recognize, value, verify};
use nom::multi::{many0, many1, separated_list0, separated_list1};
use nom::number::complete::recognize_float;
use nom::sequence::{preceded, terminated, tuple};
use nom::{IResult, InputIter, InputTake};
use nom_locate::LocatedSpan;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap};

static KEYWORDS: &[&str] = &[
    "val", "fun", "fn", "let", "in", "end", "if", "then", "else", "case", "of", "_", "datatype",
    "op", "=>", "infix", "infixr",
];

static RESERVED: &[&str] = &["|", "=", "#"];

pub struct Parser {
    infixes: RefCell<Vec<BTreeMap<u8, Vec<Symbol>>>>,
}

type Input<'a> = LocatedSpan<&'a str>;

fn current_location(input: &Input) -> Location {
    Location {
        line: input.location_line() as usize,
        column: input.get_utf8_column(),
    }
}

fn with_position<T, F>(inner_parser: F) -> impl Fn(Input) -> IResult<Input, Annot<Empty, T>>
where
    F: Fn(Input) -> IResult<Input, T>,
{
    move |i| {
        let start = current_location(&i);
        let (i, inner) = inner_parser(i)?;
        let end = current_location(&i);
        Ok((i, Annot::new(start..end, inner)))
    }
}

impl Parser {
    pub fn new() -> Self {
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
    pub fn top(&self) -> impl Fn(Input) -> IResult<Input, UntypedAst> + '_ {
        move |i| {
            let (i, _) = self.space0()(i)?;
            let (i, tops) = separated_list0(self.space1(), self.decl())(i)?;
            let (i, _) = self.space0()(i)?;
            Ok((i, AST(tops)))
        }
    }
    fn decl(&self) -> impl Fn(Input) -> IResult<Input, UntypedDeclaration> + '_ {
        move |i| {
            alt((
                self.decl_langitem(),
                self.decl_datatype(),
                self.decl_val(),
                self.decl_fun(),
                self.decl_infix(),
            ))(i)
        }
    }

    fn decl_datatype(&self) -> impl Fn(Input) -> IResult<Input, UntypedDeclaration> + '_ {
        move |i| {
            let (i, _) = tag("datatype")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, (_, name)) = self.symbol()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("=")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, constructors) = separated_list1(
                tuple((self.space0(), tag("|"), self.space0())),
                self.constructor_def(),
            )(i)?;
            Ok((i, Declaration::Datatype { name, constructors }))
        }
    }

    fn decl_val(&self) -> impl Fn(Input) -> IResult<Input, UntypedDeclaration> + '_ {
        move |i| {
            let (i, _) = tag("val")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, pattern) = self.pattern()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("=")(i)?;
            let (i, _) = self.space0()(i)?;
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
    fn decl_langitem(&self) -> impl Fn(Input) -> IResult<Input, UntypedDeclaration> + '_ {
        move |i| {
            let (i, _) = tag("__lang_item((")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("")(i)?;
            let (i, name) = self.decl_langitem_name()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("))__")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, decl) = self.decl()(i)?;
            Ok((
                i,
                Declaration::LangItem {
                    name,
                    decl: Box::new(decl),
                },
            ))
        }
    }
    fn decl_langitem_name(&self) -> impl Fn(Input) -> IResult<Input, LangItem> + '_ {
        move |i| {
            let (i, name) = alt((
                map(complete(tag("bool")), |_| LangItem::Bool),
                map(complete(tag("stringEq")), |_| LangItem::StringEq),
                map(complete(tag("stringNeq")), |_| LangItem::StringNeq),
                map(complete(tag("stringGt")), |_| LangItem::StringGt),
                map(complete(tag("stringGe")), |_| LangItem::StringGe),
                map(complete(tag("stringLt")), |_| LangItem::StringLt),
                map(complete(tag("stringLe")), |_| LangItem::StringLe),
                map(complete(tag("string")), |_| LangItem::String),
            ))(i)?;
            Ok((i, name))
        }
    }

    fn decl_fun(&self) -> impl Fn(Input) -> IResult<Input, UntypedDeclaration> + '_ {
        move |i| {
            let (i, _) = tag("fun")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, cs) = separated_list0(
                tuple((self.space0(), tag("|"), self.space0())),
                map(
                    tuple((
                        self.decl_funbind(),
                        self.space0(),
                        tag("="),
                        self.space0(),
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
                    return Err(nom::Err::Error(nom::error::Error {
                        input: i,
                        code: nom::error::ErrorKind::Tag,
                    }));
                }
                clauses.push((params, expr))
            }
            Ok((i, Declaration::D(DerivedDeclaration::Fun { name, clauses })))
        }
    }

    fn decl_funbind(&self) -> impl Fn(Input) -> IResult<Input, (Symbol, Vec<UntypedPattern>)> + '_ {
        move |i| {
            map(
                tuple((
                    self.op_symbol_eq(),
                    self.space0(),
                    separated_list0(self.space1(), self.pattern_atmic()),
                )),
                |((_, name), _, pats)| (name, pats),
            )(i)
        }
    }

    fn constructor_def(&self) -> impl Fn(Input) -> IResult<Input, (Symbol, Option<Type>)> + '_ {
        move |i| {
            let (i, (_i, name)) = self.symbol()(i)?;
            let (i, param) = opt(complete(map(
                tuple((self.space1(), tag("of"), self.space1(), self.typename())),
                |(_, _, _, ty)| ty,
            )))(i)?;

            Ok((i, (name, param)))
        }
    }

    fn decl_infix(&self) -> impl Fn(Input) -> IResult<Input, UntypedDeclaration> + '_ {
        move |i| {
            let (i, _) = tag("infix")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, priority) = opt(digit1)(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, names) = separated_list0(self.space1(), self.symbol_eq())(i)?;
            let names = names.into_iter().map(|(_, name)| name).collect::<Vec<_>>();
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

    fn expr(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
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

    fn expr1(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        move |i| {
            alt((
                self.expr1_tuple(),
                self.expr1_unit(),
                self.expr1_paren(),
                self.expr1_float(),
                self.expr1_int(),
                self.expr1_char(),
                self.expr1_string(),
                self.expr1_sym(),
                self.expr1_builtincall(),
                self.expr1_externcall(),
            ))(i)
        }
    }

    fn expr_bind(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            self.with_scope(|| {
                let (i, _) = tag("let")(i)?;
                let (i, _) = self.space1()(i)?;
                let (i, binds) = separated_list1(self.space1(), self.decl())(i)?;
                let (i, _) = self.space1()(i)?;
                let (i, _) = tag("in")(i)?;
                let (i, _) = self.space1()(i)?;
                let (i, ret) = self.expr()(i)?;
                let (i, _) = self.space1()(i)?;
                let (i, _) = tag("end")(i)?;
                let inner = ExprKind::Binds {
                    binds,
                    ret: ret.boxed(),
                };
                Ok((i, inner))
            })
        })
    }

    fn expr_fun(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            let (i, _) = tag("fn")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, (_, param)) = self.symbol()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("=>")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, body) = self.expr()(i)?;

            let inner = ExprKind::Fn {
                param,
                body: body.boxed(),
            };
            Ok((i, inner))
        })
    }

    fn expr_if(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            let (i, _) = tag("if")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, cond) = self.expr()(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, _) = tag("then")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, then) = self.expr()(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, _) = tag("else")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, else_) = self.expr()(i)?;

            let inner = ExprKind::D(DerivedExprKind::If {
                cond: cond.boxed(),
                then: then.boxed(),
                else_: else_.boxed(),
            });
            Ok((i, inner))
        })
    }

    fn expr_case(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            let (i, _) = tag("case")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, cond) = self.expr()(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, _) = tag("of")(i)?;
            let (i, _) = self.space1()(i)?;
            let (i, clauses) = separated_list0(
                tuple((self.space0(), tag("|"), self.space0())),
                map(
                    tuple((
                        self.pattern(),
                        self.space0(),
                        tag("=>"),
                        self.space0(),
                        self.expr(),
                    )),
                    |(pat, _, _, _, expr)| (pat, expr),
                ),
            )(i)?;
            let inner = ExprKind::Case {
                cond: cond.boxed(),
                clauses,
            };
            Ok((i, inner))
        })
    }

    // treat all of the infix operators and applications, i.e. sequeces of expressions
    fn expr_infix_and_app(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        move |i| {
            // TODO: support 1+1
            let (i, mixed) = many1(map(tuple((self.space0(), self.expr1())), |(_, e)| e))(i)?;
            #[derive(Debug)]
            enum Mixed {
                E(UntypedExpr),
                Fix(Span, u8, Symbol),
            }
            use Mixed::*;
            // find infixes
            let mixed = mixed
                .into_iter()
                .map(|mut e| match e.inner {
                    ExprKind::Symbol { name } => {
                        for (f, table) in self.get_table() {
                            if table.contains(&name) {
                                return Fix(e.span, f, name);
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
                (E(e1), E(e2)) => {
                    let start = e1.span.start;
                    let end = e2.span.end;
                    (
                        E(Expr::new(
                            start..end,
                            ExprKind::App {
                                fun: e1.boxed(),
                                arg: e2.boxed(),
                            },
                        )),
                        None,
                    )
                }
                (m1, m2) => (m1, Some(m2)),
            });

            // reduce infixes
            fn reduce_infixl_n(n: u8, mixed: Vec<Mixed>) -> Vec<Mixed> {
                use Mixed::*;
                map_window3(mixed, |m1, m2, m3| match (m1, m2, m3) {
                    (E(l), Fix(op_span, fixty, op), E(r)) if fixty == n => {
                        let start = l.span.start;
                        let end = r.span.end;
                        (
                            E(Expr::new(
                                start..end,
                                ExprKind::App {
                                    fun: Expr::new(op_span, ExprKind::Symbol { name: op }).boxed(),
                                    arg: Expr::new(
                                        start..end,
                                        ExprKind::Tuple { tuple: vec![l, r] },
                                    )
                                    .boxed(),
                                },
                            )),
                            None,
                        )
                    }
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
    fn expr1_sym(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        move |i| {
            // = is allowed to be used in expression exceptionally
            map(
                alt((
                    self.symbol(),
                    // perhups this is not needed
                    map(tag("="), |i: Input| {
                        let cur = current_location(&i);
                        (cur..cur, Symbol::new(*i.fragment()))
                    }),
                )),
                |(span, name)| Expr::new(span, ExprKind::Symbol { name }),
            )(i)
        }
    }

    fn expr1_int(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            map(digit1, |s: Input| ExprKind::Literal {
                value: Literal::Int(s.parse().unwrap()),
            })(i)
        })
    }

    fn expr1_float(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            map(
                verify(recognize_float, |s: &Input| s.contains('.')),
                |s: Input| ExprKind::Literal {
                    value: Literal::Real(s.parse().unwrap()),
                },
            )(i)
        })
    }

    fn expr1_char(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            let (i, _) = tag("#")(i)?;
            let (i, s) = self.string_literal()(i)?;
            assert_eq!(s.iter().count(), 1);
            let c = s.into_iter().next().unwrap();
            Ok((
                i,
                ExprKind::Literal {
                    value: Literal::Char(c),
                },
            ))
        })
    }

    fn expr1_string(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            let (i, s) = self.string_literal()(i)?;
            Ok((i, ExprKind::D(DerivedExprKind::String { value: s })))
        })
    }

    fn string_literal(&self) -> impl Fn(Input) -> IResult<Input, Vec<u32>> + '_ {
        move |i| {
            let (i, _) = tag("\"")(i)?;
            let mut s = vec![];
            let mut chars = i.iter_elements();
            let mut count = 0usize;
            let eeof = Err(nom::Err::Error(nom::error::Error {
                input: i,
                code: nom::error::ErrorKind::Eof,
            }));
            let etag = Err(nom::Err::Error(nom::error::Error {
                input: i,
                code: nom::error::ErrorKind::Tag,
            }));
            while let Some(c) = chars.next() {
                count += 1;
                match c {
                    '\\' => {
                        let c = match chars.next() {
                            Some(c) => c,
                            None => return eeof,
                        };
                        count += 1;
                        match c {
                            'a' => s.push(7),
                            'b' => s.push(8),
                            't' => s.push(9),
                            'n' => s.push(10),
                            'v' => s.push(11),
                            'f' => s.push(12),
                            'r' => s.push(13),
                            '"' => s.push('"' as u32),
                            '\\' => s.push('\\' as u32),
                            // \^{Ctrl}
                            '^' => match chars.next() {
                                Some(c) => {
                                    let c = c as u32;
                                    count += 1;
                                    if 64 <= c && c <= 95 {
                                        s.push(c - 64)
                                    } else {
                                        return etag;
                                    }
                                }
                                None => return eeof,
                            },
                            // \{ddd}
                            c1 @ '0'..='9' => {
                                let c1 = c1 as u32 - '0' as u32;
                                let c2 = match chars.next() {
                                    Some(c) if ('0'..='9').contains(&c) => c as u32 - '0' as u32,
                                    Some(_) => return etag,
                                    None => return eeof,
                                };
                                let c3 = match chars.next() {
                                    Some(c) if ('0'..='9').contains(&c) => c as u32 - '0' as u32,
                                    Some(_) => return etag,
                                    None => return eeof,
                                };
                                count += 2;
                                let d = c1 * 100 + c2 * 10 + c3;
                                s.push(d);
                            }
                            // \u{xxxx}
                            'u' => {
                                let hex = "0123456789abcdef";
                                let c1 = match chars
                                    .next()
                                    .and_then(|c| hex.find(c.to_ascii_lowercase()))
                                {
                                    Some(d) => d as u32,
                                    None => return eeof,
                                };
                                let c2 = match chars
                                    .next()
                                    .and_then(|c| hex.find(c.to_ascii_lowercase()))
                                {
                                    Some(d) => d as u32,
                                    None => return eeof,
                                };
                                let c3 = match chars
                                    .next()
                                    .and_then(|c| hex.find(c.to_ascii_lowercase()))
                                {
                                    Some(d) => d as u32,
                                    None => return eeof,
                                };
                                let c4 = match chars
                                    .next()
                                    .and_then(|c| hex.find(c.to_ascii_lowercase()))
                                {
                                    Some(d) => d as u32,
                                    None => return eeof,
                                };
                                count += 4;
                                let d = c1 * 16 * 16 * 16 + c2 * 16 * 16 + c3 * 16 + c4;
                                s.push(d);
                            }
                            // \f... f\
                            c if c.is_whitespace() => {
                                let mut n = None;
                                while let Some(c) = chars.next() {
                                    count += 1;
                                    if !c.is_whitespace() {
                                        n = Some(c);
                                        break;
                                    }
                                }
                                match n {
                                    Some('\\') => (),
                                    Some(_) => return etag,
                                    None => return eeof,
                                };
                            }
                            _ => {
                                return Err(nom::Err::Error(nom::error::Error {
                                    input: i,
                                    code: nom::error::ErrorKind::Tag,
                                }))
                            }
                        }
                    }
                    '"' => break,
                    c => s.push(c as u32),
                }
            }
            let (i, _) = i.take_split(count);
            Ok((i, s))
        }
    }

    fn expr1_paren(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, e) = self.expr()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((i, e))
        }
    }

    fn expr1_tuple(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = self.space0()(i)?;
            let sep = tuple((self.space0(), tag(","), self.space0()));
            let (i, es) = many1(map(tuple((self.expr(), sep)), |(e, _)| e))(i)?;
            let (i, e) = self.expr()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag(")")(i)?;

            let mut es = es;
            es.push(e);
            Ok((i, ExprKind::Tuple { tuple: es }))
        })
    }

    fn expr1_unit(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            value(
                ExprKind::Tuple { tuple: vec![] },
                tuple((tag("("), self.space0(), tag(")"))),
            )(i)
        })
    }

    fn expr1_builtincall(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            let (i, _) = tag("_builtincall")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("\"")(i)?;
            let (i, fun) = map_res(alphanumeric1, |name: Input| match &name as &str {
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
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, args) =
                separated_list1(tuple((self.space0(), tag(","), self.space0())), self.expr())(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((i, ExprKind::BuiltinCall { fun, args }))
        })
    }

    /// `_externcall ("module"."fun": (arg, ty) -> retty) (arg, s)`
    fn expr1_externcall(&self) -> impl Fn(Input) -> IResult<Input, UntypedExpr> + '_ {
        with_position(move |i| {
            fn name_parser(i: Input) -> IResult<Input, Input> {
                let allowed = recognize(many1(nom::character::complete::none_of("\"")));
                preceded(tag("\""), terminated(allowed, tag("\"")))(i)
            }
            let (i, _) = tag("_externcall")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, module) = map(name_parser, |i| i.to_string())(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag(".")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, fun) = map(name_parser, |i| i.to_string())(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag(":")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, argty) = separated_list1(
                tuple((self.space0(), tag(","), self.space0())),
                self.typename(),
            )(i)?;
            let (i, _) = tag(")")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("->")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, retty) = self.typename()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag(")")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("(")(i)?;
            let (i, args) =
                separated_list1(tuple((self.space0(), tag(","), self.space0())), self.expr())(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((
                i,
                ExprKind::ExternCall {
                    module,
                    fun,
                    args,
                    argty,
                    retty,
                },
            ))
        })
    }

    fn typename(&self) -> impl Fn(Input) -> IResult<Input, Type> + '_ {
        move |i| self.typename0()(i)
    }

    fn typename0(&self) -> impl Fn(Input) -> IResult<Input, Type> + '_ {
        move |i| alt((complete(self.typename0_fun()), self.typename1()))(i)
    }

    fn typename1(&self) -> impl Fn(Input) -> IResult<Input, Type> + '_ {
        move |i| alt((self.typename1_tuple(), self.typename2()))(i)
    }

    fn typename2(&self) -> impl Fn(Input) -> IResult<Input, Type> + '_ {
        move |i| alt((self.typename2_paren(), self.typename2_datatype()))(i)
    }

    fn typename0_fun(&self) -> impl Fn(Input) -> IResult<Input, Type> + '_ {
        move |i| {
            let (i, arg) = self.typename1()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag("->")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, ret) = self.typename()(i)?;
            Ok((i, Type::Fun(Box::new(arg), Box::new(ret))))
        }
    }

    fn typename1_tuple(&self) -> impl Fn(Input) -> IResult<Input, Type> + '_ {
        move |i| {
            let sep = tuple((self.space0(), tag("*"), self.space0()));

            let (i, tys) = many1(map(tuple((self.typename2(), sep)), |(ty, _)| ty))(i)?;
            let (i, ty) = self.typename2()(i)?;

            let mut tys = tys;
            tys.push(ty);
            Ok((i, Type::Tuple(tys)))
        }
    }

    fn typename2_paren(&self) -> impl Fn(Input) -> IResult<Input, Type> + '_ {
        move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, ty) = self.typename()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag(")")(i)?;
            Ok((i, ty))
        }
    }

    fn typename2_datatype(&self) -> impl Fn(Input) -> IResult<Input, Type> + '_ {
        move |i| {
            map(self.symbol(), |(_, name)| match name.0.as_str() {
                "unit" => Type::Tuple(vec![]),
                "real" => Type::Real,
                "int" => Type::Int,
                "char" => Type::Char,
                _ => Type::Datatype(name),
            })(i)
        }
    }

    fn symbol_eq(&self) -> impl Fn(Input) -> IResult<Input, (Span, Symbol)> + '_ {
        move |i| alt((self.symbol_alphanumeric(), self.symbol_symbolic_eq()))(i)
    }

    fn symbol(&self) -> impl Fn(Input) -> IResult<Input, (Span, Symbol)> + '_ {
        move |i| alt((self.symbol_alphanumeric(), self.symbol_symbolic()))(i)
    }

    fn op_symbol_eq(&self) -> impl Fn(Input) -> IResult<Input, (Span, Symbol)> + '_ {
        move |i| alt((self.op_symbol_alphanumeric(), self.op_symbol_symbolic_eq()))(i)
    }

    fn op_symbol_alphanumeric(&self) -> impl Fn(Input) -> IResult<Input, (Span, Symbol)> + '_ {
        move |i| {
            let start = current_location(&i);
            let (i, _) = opt(tuple((tag("op"), self.space1())))(i)?;
            let (i, (loc, sym)) = self.symbol_alphanumeric()(i)?;
            Ok((i, (start..loc.end, sym)))
        }
    }

    fn op_symbol_symbolic_eq(&self) -> impl Fn(Input) -> IResult<Input, (Span, Symbol)> + '_ {
        move |i| {
            let start = current_location(&i);
            let (i, _) = opt(tuple((tag("op"), self.space0())))(i)?;
            let cur = current_location(&i);
            let (i, (loc, sym)) = alt((
                self.symbol_symbolic(),
                // FIXME: calculate correct span
                value((cur..cur, Symbol::new("=")), tag("=")),
            ))(i)?;
            Ok((i, (start..loc.end, sym)))
        }
    }

    fn symbol_alphanumeric(&self) -> impl Fn(Input) -> IResult<Input, (Span, Symbol)> + '_ {
        move |i| {
            // FIXME: collect syntax is [a-zA-Z'_][a-zA-Z'_0-9]*
            let start = current_location(&i);
            let (i, sym) = verify(alphanumeric1, |s: &Input| !KEYWORDS.contains(&s.fragment()))(i)?;
            let end = current_location(&i);
            Ok((i, (start..end, Symbol::new(sym.to_string()))))
        }
    }

    fn symbol_symbolic_eq(&self) -> impl Fn(Input) -> IResult<Input, (Span, Symbol)> + '_ {
        move |i| {
            let cur = current_location(&i);
            alt((
                self.symbol_symbolic(),
                // FIXME: calculate correct span
                value((cur..cur, Symbol::new("=")), tag("=")),
            ))(i)
        }
    }

    fn symbol_symbolic(&self) -> impl Fn(Input) -> IResult<Input, (Span, Symbol)> + '_ {
        move |i| {
            let symbolic1 = recognize(many1(nom::character::complete::one_of(
                "!%&$#+-/:<=>?@\\~'^|*",
            )));

            let start = current_location(&i);
            let (i, sym) = verify(symbolic1, |s: &Input| {
                !KEYWORDS.contains(s.fragment()) && !RESERVED.contains(&s)
            })(i)?;
            let end = current_location(&i);
            Ok((i, (start..end, Symbol::new(sym.to_string()))))
        }
    }

    fn space0(&self) -> impl Fn(Input) -> IResult<Input, ()> + '_ {
        move |i| {
            map(
                many0(alt((map(multispace1, |_| ()), self.comment()))),
                |_| (),
            )(i)
        }
    }

    fn space1(&self) -> impl Fn(Input) -> IResult<Input, ()> + '_ {
        move |i| {
            map(
                many1(alt((map(multispace1, |_| ()), self.comment()))),
                |_| (),
            )(i)
        }
    }

    fn comment(&self) -> impl Fn(Input) -> IResult<Input, ()> + '_ {
        move |i| {
            let (i, _) = tag("(*")(i)?;
            let mut nest = 1;
            let mut chars = i.iter_elements();
            let mut count = 0;
            while let Some(c) = chars.next() {
                count += 1;
                match c {
                    '(' => {
                        if let Some('*') = chars.next() {
                            count += 1;
                            nest += 1;
                        }
                    }
                    '*' => {
                        if let Some(')') = chars.next() {
                            count += 1;
                            nest -= 1;
                            if nest == 0 {
                                break;
                            }
                        }
                    }
                    _ => (),
                }
            }
            if nest != 0 {
                panic!("parser reached EOF while pasing comment; maybe (* is not closed?");
            }
            let (i, _) = i.take_split(count);
            Ok((i, ()))
        }
    }

    fn pattern(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        move |i| alt((self.pattern_constructor(), self.pattern_atmic()))(i)
    }

    fn pattern_atmic(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        move |i| {
            alt((
                self.pattern_char(),
                self.pattern_string(),
                self.pattern_int(),
                self.pattern_tuple(),
                self.pattern_var(),
                self.pattern_wildcard(),
                self.pattern_unit(),
                self.pattern_paren(),
            ))(i)
        }
    }

    fn pattern_int(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        with_position(move |i| {
            map(digit1, |s: Input| PatternKind::Constant {
                value: s.parse().unwrap(),
            })(i)
        })
    }

    fn pattern_char(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        with_position(move |i| {
            let (i, _) = tag("#")(i)?;
            let (i, s) = self.string_literal()(i)?;
            assert_eq!(s.iter().count(), 1);
            let value = s.into_iter().next().unwrap();
            Ok((i, PatternKind::Char { value }))
        })
    }

    fn pattern_string(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        with_position(move |i| {
            map(self.string_literal(), |value| {
                PatternKind::D(DerivedPatternKind::String { value })
            })(i)
        })
    }

    fn pattern_tuple(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        with_position(move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = self.space0()(i)?;
            let sep = tuple((self.space0(), tag(","), self.space0()));
            let (i, es) = many1(map(tuple((self.pattern(), sep)), |(e, _)| e))(i)?;
            let (i, e) = self.pattern()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, _) = tag(")")(i)?;

            let mut tuple = es;
            tuple.push(e);
            Ok((i, PatternKind::Tuple { tuple }))
        })
    }

    fn pattern_unit(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        with_position(move |i| {
            value(
                PatternKind::Tuple { tuple: vec![] },
                tuple((tag("("), self.space0(), tag(")"))),
            )(i)
        })
    }

    // require constructor to have arg for now.
    // constructor withouth arg is parsed as variable and
    // will be converted in later phases
    fn pattern_constructor(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        with_position(move |i| {
            let (i, (_, name)) = self.symbol()(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, arg) = self.pattern_atmic()(i)?;
            Ok((
                i,
                PatternKind::Constructor {
                    name,
                    arg: Some(Box::new(arg)),
                },
            ))
        })
    }

    fn pattern_var(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        move |i| {
            map(self.symbol(), |(span, name)| {
                Pattern::new(span, PatternKind::Variable { name })
            })(i)
        }
    }

    fn pattern_wildcard(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        with_position(move |i| value(PatternKind::Wildcard {}, tag("_"))(i))
    }

    fn pattern_paren(&self) -> impl Fn(Input) -> IResult<Input, UntypedPattern> + '_ {
        move |i| {
            let (i, _) = tag("(")(i)?;
            let (i, _) = self.space0()(i)?;
            let (i, e) = self.pattern()(i)?;
            let (i, _) = self.space0()(i)?;
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
    use nom::InputTake;
    let input = "true";
    let ret = Parser::new().expr_infix_and_app()(Input::new(input)).unwrap();
    let (input_remaining, _) = Input::new(input).take_split(input.len());
    assert_eq!(
        ret,
        (
            input_remaining,
            Expr {
                ty: Empty {},
                span: Location::new(1, 1)..Location::new(1, 5),
                inner: ExprKind::Symbol {
                    name: Symbol::new("true")
                }
            }
        )
    )
}

#[test]
fn test_expr_infix_and_app2() {
    use nom::InputTake;

    let input = "f arg";
    let ret = Parser::new().expr_infix_and_app()(Input::new(input)).unwrap();
    let (input_remaining, _) = Input::new(input).take_split(input.len());
    assert_eq!(
        ret,
        (
            input_remaining,
            Expr {
                ty: Empty {},
                span: Location::new(1, 1)..Location::new(1, 6),
                inner: ExprKind::App {
                    fun: Expr {
                        ty: Empty {},
                        span: Location::new(1, 1)..Location::new(1, 2),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("f"),
                        }
                    }
                    .boxed(),
                    arg: Expr {
                        ty: Empty {},
                        span: Location::new(1, 3)..Location::new(1, 6),
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

impl Pass<String, TypeError> for Parser {
    type Target = UntypedAst;

    fn trans(&mut self, input: String, _: &Config) -> std::result::Result<Self::Target, TypeError> {
        let input = Input::new(&input);
        match all_consuming(self.top())(input) {
            Ok((_, iresult)) => Ok(iresult),
            Err(e) => Err(TypeError::ParseError(format!("{e}"))),
        }
    }
}
