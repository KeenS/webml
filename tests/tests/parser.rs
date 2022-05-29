use nom_locate::LocatedSpan;
use webml::ast::{
    Declaration, DerivedDeclaration, DerivedExprKind, Empty, Expr, ExprKind, LangItem, Location,
    Pattern, PatternKind, Type, UntypedAst, AST, BIF,
};
use webml::prim::*;
use webml::Parser;

fn parse(input: &str) -> Result<UntypedAst, nom::Err<nom::error::Error<LocatedSpan<&str>>>> {
    let parser = Parser::default();
    let input = LocatedSpan::new(input);
    let (_, iresult) = nom::combinator::all_consuming(parser.top())(input)?;
    Ok(iresult)
}

#[test]
fn parse_char() {
    let input = r##"val x = #"a""##;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 13),
                inner: ExprKind::Literal {
                    value: Literal::Char('a' as u32),
                }
            },
        },])
    )
}

#[test]
fn parse_string() {
    let input = r#"val x = "abc\n\\\^J\097\u03BA\
	\next line""#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(2, 13),
                inner: ExprKind::D(DerivedExprKind::String {
                    value: "abc\n\\\naκnext line".chars().map(|c| c as u32).collect()
                })
            },
        },])
    )
}

#[test]
fn parse_int() {
    let input = r#"val x = 1"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 10),
                inner: ExprKind::Literal {
                    value: Literal::Int(1),
                }
            },
        },])
    )
}

#[test]
fn parse_float() {
    let input = r#"val x = 1.0"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 12),
                inner: ExprKind::Literal {
                    value: Literal::Real(1.0),
                }
            },
        },])
    )
}

#[test]
fn parse_bool_true() {
    let input = r#"val x = true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 13),
                inner: ExprKind::Symbol {
                    name: Symbol::new("true")
                }
            },
        },])
    )
}

#[test]
fn parse_bool_false() {
    let input = r#"val x = false"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 14),
                inner: ExprKind::Symbol {
                    name: Symbol::new("false")
                }
            },
        },])
    )
}

#[test]
fn parse_unit() {
    let input = r#"val x = ()"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 11),
                inner: ExprKind::Tuple { tuple: vec![] }
            }
        }])
    )
}

#[test]
fn parse_tuple() {
    let input = r#"val x = (x, y)"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 15),
                inner: ExprKind::Tuple {
                    tuple: vec![
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 10)..Location::new(1, 11),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("x")
                            }
                        },
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 13)..Location::new(1, 14),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("y")
                            }
                        }
                    ]
                }
            }
        }])
    )
}

#[test]
fn parse_seq() {
    let input = r#"val x = (x; y)"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 15),
                inner: ExprKind::D(DerivedExprKind::Seq {
                    seq: vec![
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 10)..Location::new(1, 11),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("x")
                            }
                        },
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 13)..Location::new(1, 14),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("y")
                            }
                        }
                    ]
                })
            }
        }])
    )
}

#[test]
fn parse_apply() {
    let input = r#"val x = f x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 12),
                inner: ExprKind::App {
                    fun: Expr {
                        ty: Empty {},
                        span: Location::new(1, 9)..Location::new(1, 10),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("f")
                        }
                    }
                    .boxed(),
                    arg: Expr {
                        ty: Empty {},
                        span: Location::new(1, 11)..Location::new(1, 12),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("x")
                        }
                    }
                    .boxed()
                }
            }
        }])
    )
}

#[test]
fn parse_apply_tuple() {
    let input = r#"val x = f(x, y)"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 16),
                inner: ExprKind::App {
                    fun: Expr {
                        ty: Empty {},
                        span: Location::new(1, 9)..Location::new(1, 10),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("f")
                        }
                    }
                    .boxed(),
                    arg: Expr {
                        ty: Empty {},
                        span: Location::new(1, 10)..Location::new(1, 16),
                        inner: ExprKind::Tuple {
                            tuple: vec![
                                Expr {
                                    ty: Empty {},
                                    span: Location::new(1, 11)..Location::new(1, 12),
                                    inner: ExprKind::Symbol {
                                        name: Symbol::new("x")
                                    }
                                },
                                Expr {
                                    ty: Empty {},
                                    span: Location::new(1, 14)..Location::new(1, 15),
                                    inner: ExprKind::Symbol {
                                        name: Symbol::new("y")
                                    }
                                }
                            ]
                        }
                    }
                    .boxed()
                }
            }
        }])
    )
}
#[test]
fn parse_binop() {
    let input = r#"infix 6 + val x = 1 + 2"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infix {
                priority: Some(6),
                names: vec![Symbol::new("+")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 15)..Location::new(1, 16),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 19)..Location::new(1, 24),
                    inner: ExprKind::App {
                        fun: Expr {
                            ty: Empty {},
                            span: Location::new(1, 21)..Location::new(1, 22),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("+")
                            }
                        }
                        .boxed(),
                        arg: Expr {
                            ty: Empty {},
                            span: Location::new(1, 19)..Location::new(1, 24),
                            inner: ExprKind::Tuple {
                                tuple: vec![
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 19)..Location::new(1, 20),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(1),
                                        }
                                    },
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 23)..Location::new(1, 24),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(2),
                                        }
                                    }
                                ]
                            }
                        }
                        .boxed()
                    }
                }
            },
        ])
    )
}

#[test]
fn parse_binopr() {
    let input = r#"infixr 6 + val x = 1 + 2"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infixr {
                priority: Some(6),
                names: vec![Symbol::new("+")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 16)..Location::new(1, 17),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 20)..Location::new(1, 25),
                    inner: ExprKind::App {
                        fun: Expr {
                            ty: Empty {},
                            span: Location::new(1, 22)..Location::new(1, 23),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("+")
                            }
                        }
                        .boxed(),
                        arg: Expr {
                            ty: Empty {},
                            span: Location::new(1, 20)..Location::new(1, 25),
                            inner: ExprKind::Tuple {
                                tuple: vec![
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 20)..Location::new(1, 21),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(1),
                                        }
                                    },
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 24)..Location::new(1, 25),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(2),
                                        }
                                    }
                                ]
                            }
                        }
                        .boxed()
                    }
                }
            },
        ])
    )
}

#[test]
fn parse_nofix() {
    let input = r#"infix 6 + nofix + val x = +(1, 2)"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infix {
                priority: Some(6),
                names: vec![Symbol::new("+")],
            }),
            Declaration::D(DerivedDeclaration::Nofix {
                names: vec![Symbol::new("+")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 23)..Location::new(1, 24),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 27)..Location::new(1, 34),
                    inner: ExprKind::App {
                        fun: Expr {
                            ty: Empty {},
                            span: Location::new(1, 27)..Location::new(1, 28),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("+")
                            }
                        }
                        .boxed(),
                        arg: Expr {
                            ty: Empty {},
                            span: Location::new(1, 28)..Location::new(1, 34),
                            inner: ExprKind::Tuple {
                                tuple: vec![
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 29)..Location::new(1, 30),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(1),
                                        }
                                    },
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 32)..Location::new(1, 33),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(2),
                                        }
                                    }
                                ]
                            }
                        }
                        .boxed()
                    }
                }
            },
        ])
    )
}

#[test]
fn parse_op() {
    let input = r#"infix 6 + val x = op+(1, 2)"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infix {
                priority: Some(6),
                names: vec![Symbol::new("+")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 15)..Location::new(1, 16),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 19)..Location::new(1, 28),
                    inner: ExprKind::App {
                        fun: Expr {
                            ty: Empty {},
                            span: Location::new(1, 19)..Location::new(1, 22),
                            inner: ExprKind::D(DerivedExprKind::Op {
                                name: Symbol::new("+")
                            })
                        }
                        .boxed(),
                        arg: Expr {
                            ty: Empty {},
                            span: Location::new(1, 22)..Location::new(1, 28),
                            inner: ExprKind::Tuple {
                                tuple: vec![
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 23)..Location::new(1, 24),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(1),
                                        }
                                    },
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 26)..Location::new(1, 27),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(2),
                                        }
                                    }
                                ]
                            }
                        }
                        .boxed()
                    }
                }
            },
        ])
    )
}
#[test]
fn parse_binop_space() {
    let input = r#"infix 6 + val x = 1+2"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infix {
                priority: Some(6),
                names: vec![Symbol::new("+")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 15)..Location::new(1, 16),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 19)..Location::new(1, 22),
                    inner: ExprKind::App {
                        fun: Expr {
                            ty: Empty {},
                            span: Location::new(1, 20)..Location::new(1, 21),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("+")
                            }
                        }
                        .boxed(),
                        arg: Expr {
                            ty: Empty {},
                            span: Location::new(1, 19)..Location::new(1, 22),
                            inner: ExprKind::Tuple {
                                tuple: vec![
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 19)..Location::new(1, 20),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(1),
                                        }
                                    },
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 21)..Location::new(1, 22),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(2),
                                        }
                                    }
                                ]
                            }
                        }
                        .boxed()
                    }
                }
            },
        ])
    )
}

#[test]
fn parse_binop_assoc() {
    let input = r#"infix 6 + val x = 1 + 2 + 3"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infix {
                priority: Some(6),
                names: vec![Symbol::new("+")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 15)..Location::new(1, 16),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 19)..Location::new(1, 28),
                    inner: ExprKind::App {
                        fun: Expr {
                            ty: Empty {},
                            span: Location::new(1, 25)..Location::new(1, 26),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("+"),
                            }
                        }
                        .boxed(),
                        arg: Expr {
                            ty: Empty {},
                            span: Location::new(1, 19)..Location::new(1, 28),
                            inner: ExprKind::Tuple {
                                tuple: vec![
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 19)..Location::new(1, 24),
                                        inner: ExprKind::App {
                                            fun: Expr {
                                                ty: Empty {},
                                                span: Location::new(1, 21)..Location::new(1, 22),
                                                inner: ExprKind::Symbol {
                                                    name: Symbol::new("+"),
                                                }
                                            }
                                            .boxed(),
                                            arg: Expr {
                                                ty: Empty {},
                                                span: Location::new(1, 19)..Location::new(1, 24),
                                                inner: ExprKind::Tuple {
                                                    tuple: vec![
                                                        Expr {
                                                            ty: Empty {},
                                                            span: Location::new(1, 19)
                                                                ..Location::new(1, 20),
                                                            inner: ExprKind::Literal {
                                                                value: Literal::Int(1),
                                                            }
                                                        },
                                                        Expr {
                                                            ty: Empty {},
                                                            span: Location::new(1, 23)
                                                                ..Location::new(1, 24),
                                                            inner: ExprKind::Literal {
                                                                value: Literal::Int(2),
                                                            }
                                                        }
                                                    ]
                                                }
                                            }
                                            .boxed()
                                        }
                                    },
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 27)..Location::new(1, 28),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(3),
                                        }
                                    }
                                ]
                            }
                        }
                        .boxed()
                    }
                }
            },
        ])
    )
}

#[test]
fn parse_binopr_assoc() {
    let input = r#"infixr 6 + val x = 1 + 2 + 3"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infixr {
                priority: Some(6),
                names: vec![Symbol::new("+")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 16)..Location::new(1, 17),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 20)..Location::new(1, 29),
                    inner: ExprKind::App {
                        fun: Expr {
                            ty: Empty {},
                            span: Location::new(1, 22)..Location::new(1, 23),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("+"),
                            }
                        }
                        .boxed(),
                        arg: Expr {
                            ty: Empty {},
                            span: Location::new(1, 20)..Location::new(1, 29),
                            inner: ExprKind::Tuple {
                                tuple: vec![
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 20)..Location::new(1, 21),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(1),
                                        }
                                    },
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 24)..Location::new(1, 29),
                                        inner: ExprKind::App {
                                            fun: Expr {
                                                ty: Empty {},
                                                span: Location::new(1, 26)..Location::new(1, 27),
                                                inner: ExprKind::Symbol {
                                                    name: Symbol::new("+"),
                                                }
                                            }
                                            .boxed(),
                                            arg: Expr {
                                                ty: Empty {},
                                                span: Location::new(1, 24)..Location::new(1, 29),
                                                inner: ExprKind::Tuple {
                                                    tuple: vec![
                                                        Expr {
                                                            ty: Empty {},
                                                            span: Location::new(1, 24)
                                                                ..Location::new(1, 25),
                                                            inner: ExprKind::Literal {
                                                                value: Literal::Int(2),
                                                            }
                                                        },
                                                        Expr {
                                                            ty: Empty {},
                                                            span: Location::new(1, 28)
                                                                ..Location::new(1, 29),
                                                            inner: ExprKind::Literal {
                                                                value: Literal::Int(3),
                                                            }
                                                        }
                                                    ]
                                                }
                                            }
                                            .boxed()
                                        }
                                    },
                                ]
                            }
                        }
                        .boxed()
                    }
                }
            },
        ])
    )
}

#[test]
fn parse_binop_mixed_assoc() {
    let input = r#"infix 6 + infixr 7 * val x = 1 + 2 * 3"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infix {
                priority: Some(6),
                names: vec![Symbol::new("+")],
            }),
            Declaration::D(DerivedDeclaration::Infixr {
                priority: Some(7),
                names: vec![Symbol::new("*")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 26)..Location::new(1, 27),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 30)..Location::new(1, 39),
                    inner: ExprKind::App {
                        fun: Expr {
                            ty: Empty {},
                            span: Location::new(1, 32)..Location::new(1, 33),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("+"),
                            }
                        }
                        .boxed(),
                        arg: Expr {
                            ty: Empty {},
                            span: Location::new(1, 30)..Location::new(1, 39),
                            inner: ExprKind::Tuple {
                                tuple: vec![
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 30)..Location::new(1, 31),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(1),
                                        }
                                    },
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 34)..Location::new(1, 39),
                                        inner: ExprKind::App {
                                            fun: Expr {
                                                ty: Empty {},
                                                span: Location::new(1, 36)..Location::new(1, 37),
                                                inner: ExprKind::Symbol {
                                                    name: Symbol::new("*"),
                                                }
                                            }
                                            .boxed(),
                                            arg: Expr {
                                                ty: Empty {},
                                                span: Location::new(1, 34)..Location::new(1, 39),
                                                inner: ExprKind::Tuple {
                                                    tuple: vec![
                                                        Expr {
                                                            ty: Empty {},
                                                            span: Location::new(1, 34)
                                                                ..Location::new(1, 35),
                                                            inner: ExprKind::Literal {
                                                                value: Literal::Int(2),
                                                            }
                                                        },
                                                        Expr {
                                                            ty: Empty {},
                                                            span: Location::new(1, 38)
                                                                ..Location::new(1, 39),
                                                            inner: ExprKind::Literal {
                                                                value: Literal::Int(3),
                                                            }
                                                        }
                                                    ]
                                                }
                                            }
                                            .boxed()
                                        }
                                    },
                                ]
                            }
                        }
                        .boxed()
                    }
                }
            },
        ])
    )
}

#[test]
fn parse_builtincall() {
    let input = r#"val ret = _builtincall "add" (x, y)"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 8),
                inner: PatternKind::Variable {
                    name: Symbol::new("ret"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 11)..Location::new(1, 36),
                inner: ExprKind::BuiltinCall {
                    fun: BIF::Add,
                    args: vec![
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 31)..Location::new(1, 32),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("x")
                            }
                        },
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 34)..Location::new(1, 35),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("y")
                            }
                        }
                    ]
                }
            }
        }])
    )
}

#[test]
fn parse_externcall() {
    let input = r#"val ret = _externcall ("module" . "add" : (int, int) -> int) (x, y)"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 8),
                inner: PatternKind::Variable {
                    name: Symbol::new("ret"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 11)..Location::new(1, 68),
                inner: ExprKind::ExternCall {
                    module: "module".into(),
                    fun: "add".into(),
                    args: vec![
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 63)..Location::new(1, 64),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("x")
                            }
                        },
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 66)..Location::new(1, 67),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("y")
                            }
                        }
                    ],
                    argty: vec![Type::Int, Type::Int],
                    retty: Type::Int
                }
            }
        }])
    )
}

#[test]
fn parse_binop_pref() {
    let input = r#"infix 6 + infix 7 * val x = 1 + 2 * 3"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infix {
                priority: Some(6),
                names: vec![Symbol::new("+")],
            }),
            Declaration::D(DerivedDeclaration::Infix {
                priority: Some(7),
                names: vec![Symbol::new("*")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 25)..Location::new(1, 26),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 29)..Location::new(1, 38),
                    inner: ExprKind::App {
                        fun: Expr {
                            ty: Empty {},
                            span: Location::new(1, 31)..Location::new(1, 32),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("+")
                            }
                        }
                        .boxed(),
                        arg: Expr {
                            ty: Empty {},
                            span: Location::new(1, 29)..Location::new(1, 38),
                            inner: ExprKind::Tuple {
                                tuple: vec![
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 29)..Location::new(1, 30),
                                        inner: ExprKind::Literal {
                                            value: Literal::Int(1),
                                        }
                                    },
                                    Expr {
                                        ty: Empty {},
                                        span: Location::new(1, 33)..Location::new(1, 38),
                                        inner: ExprKind::App {
                                            fun: Expr {
                                                ty: Empty {},
                                                span: Location::new(1, 35)..Location::new(1, 36),
                                                inner: ExprKind::Symbol {
                                                    name: Symbol::new("*"),
                                                }
                                            }
                                            .boxed(),
                                            arg: Expr {
                                                ty: Empty {},
                                                span: Location::new(1, 33)..Location::new(1, 38),
                                                inner: ExprKind::Tuple {
                                                    tuple: vec![
                                                        Expr {
                                                            ty: Empty {},
                                                            span: Location::new(1, 33)
                                                                ..Location::new(1, 34),
                                                            inner: ExprKind::Literal {
                                                                value: Literal::Int(2),
                                                            }
                                                        },
                                                        Expr {
                                                            ty: Empty {},
                                                            span: Location::new(1, 37)
                                                                ..Location::new(1, 38),
                                                            inner: ExprKind::Literal {
                                                                value: Literal::Int(3),
                                                            }
                                                        }
                                                    ]
                                                }
                                            }
                                            .boxed()
                                        }
                                    }
                                ],
                            }
                        }
                        .boxed()
                    }
                }
            },
        ])
    )
}

#[test]
fn parse_fn_unary() {
    let input = r#"val f = fn x => x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("f"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 18),
                inner: ExprKind::Fn {
                    param: Symbol::new("x"),
                    body: Expr {
                        ty: Empty {},
                        span: Location::new(1, 17)..Location::new(1, 18),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("x"),
                        }
                    }
                    .boxed(),
                }
            },
        },])
    )
}

#[test]
fn parse_andalso() {
    let input = r#"val x = true andalso false"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 27),
                inner: ExprKind::D(DerivedExprKind::AndAlso {
                    l: Expr {
                        ty: Empty {},
                        span: Location::new(1, 9)..Location::new(1, 13),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("true"),
                        }
                    }
                    .boxed(),
                    r: Expr {
                        ty: Empty {},
                        span: Location::new(1, 22)..Location::new(1, 27),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("false"),
                        }
                    }
                    .boxed()
                })
            }
        }])
    )
}

#[test]
fn parse_orelse() {
    let input = r#"val x = input orelse let val y = false in y end"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 48),
                inner: ExprKind::D(DerivedExprKind::OrElse {
                    l: Expr {
                        ty: Empty {},
                        span: Location::new(1, 9)..Location::new(1, 14),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("input")
                        }
                    }
                    .boxed(),
                    r: Expr {
                        ty: Empty {},
                        span: Location::new(1, 22)..Location::new(1, 48),
                        inner: ExprKind::Binds {
                            binds: vec![Declaration::Val {
                                rec: false,
                                pattern: Pattern {
                                    ty: Empty {},
                                    span: Location::new(1, 30)..Location::new(1, 31),
                                    inner: PatternKind::Variable {
                                        name: Symbol::new("y")
                                    }
                                },
                                expr: Expr {
                                    ty: Empty {},
                                    span: Location::new(1, 34)..Location::new(1, 39),
                                    inner: ExprKind::Symbol {
                                        name: Symbol::new("false")
                                    }
                                }
                            }],
                            ret: Expr {
                                ty: Empty {},
                                span: Location::new(1, 43)..Location::new(1, 44),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("y")
                                }
                            }
                            .boxed()
                        }
                    }
                    .boxed()
                })
            }
        }])
    )
}

#[test]
fn parse_andalso_orelse() {
    let input = r#"val x = true andalso false orelse true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 39),
                inner: ExprKind::D(DerivedExprKind::AndAlso {
                    l: Expr {
                        ty: Empty {},
                        span: Location::new(1, 9)..Location::new(1, 13),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("true"),
                        }
                    }
                    .boxed(),
                    r: Expr {
                        ty: Empty {},
                        span: Location::new(1, 22)..Location::new(1, 39),
                        inner: ExprKind::D(DerivedExprKind::OrElse {
                            l: Expr {
                                ty: Empty {},
                                span: Location::new(1, 22)..Location::new(1, 27),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("false"),
                                }
                            }
                            .boxed(),
                            r: Expr {
                                ty: Empty {},
                                span: Location::new(1, 35)..Location::new(1, 39),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("true"),
                                }
                            }
                            .boxed()
                        })
                    }
                    .boxed()
                })
            }
        }])
    )
}

#[test]
fn parse_val() {
    let input = r#"val x = 1"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x")
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 10),
                inner: ExprKind::Literal {
                    value: Literal::Int(1)
                }
            }
        }])
    )
}

#[test]
fn parse_val_seq() {
    let input = r#"val x = 1 val y = 2"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 5)..Location::new(1, 6),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x")
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 9)..Location::new(1, 10),
                    inner: ExprKind::Literal {
                        value: Literal::Int(1)
                    }
                }
            },
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 15)..Location::new(1, 16),
                    inner: PatternKind::Variable {
                        name: Symbol::new("y")
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 19)..Location::new(1, 20),
                    inner: ExprKind::Literal {
                        value: Literal::Int(2)
                    }
                }
            }
        ])
    )
}

#[test]
fn parse_val_seq_semi() {
    let input = r#"val x = 1;val y = 2"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 5)..Location::new(1, 6),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x")
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 9)..Location::new(1, 10),
                    inner: ExprKind::Literal {
                        value: Literal::Int(1)
                    }
                }
            },
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 15)..Location::new(1, 16),
                    inner: PatternKind::Variable {
                        name: Symbol::new("y")
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 19)..Location::new(1, 20),
                    inner: ExprKind::Literal {
                        value: Literal::Int(2)
                    }
                }
            }
        ])
    )
}

#[test]
fn parse_datatype_single() {
    let input = r#"datatype hoge = Hoge"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Datatype {
            name: Symbol::new("hoge"),
            constructors: vec![(Symbol::new("Hoge"), None)]
        },])
    )
}

#[test]
fn parse_datatype_multi() {
    let input = r#"datatype hoge = Hoge | Fuga | Piyo"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Datatype {
            name: Symbol::new("hoge"),
            constructors: vec![
                (Symbol::new("Hoge"), None),
                (Symbol::new("Fuga"), None),
                (Symbol::new("Piyo"), None)
            ]
        },])
    )
}

#[test]
fn parse_datatype_arg1() {
    let input = r#"datatype hoge = Hoge of int | Fuga of real"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Datatype {
            name: Symbol::new("hoge"),
            constructors: vec![
                (Symbol::new("Hoge"), Some(Type::Int)),
                (Symbol::new("Fuga"), Some(Type::Real))
            ]
        },])
    )
}

#[test]
fn parse_datatype_arg2() {
    let input = r#"datatype hoge = Hoge of int | Fuga of real | Piyo of bool -> unit -> int"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Datatype {
            name: Symbol::new("hoge"),
            constructors: vec![
                (Symbol::new("Hoge"), Some(Type::Int)),
                (Symbol::new("Fuga"), Some(Type::Real)),
                (
                    Symbol::new("Piyo"),
                    Some(Type::Fun(
                        Box::new(Type::Datatype(Symbol::new("bool"))),
                        Box::new(Type::Fun(
                            Box::new(Type::Tuple(vec![])),
                            Box::new(Type::Int)
                        ))
                    ))
                )
            ]
        },])
    )
}

#[test]
fn parse_datatype_tuple() {
    let input = r#"datatype hoge = Hoge of int * real"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Datatype {
            name: Symbol::new("hoge"),
            constructors: vec![(
                Symbol::new("Hoge"),
                Some(Type::Tuple(vec![Type::Int, Type::Real]))
            ),]
        },])
    )
}

#[test]
fn parse_datatype_arg3() {
    let input =
        r#"datatype hoge = Hoge of int | Fuga of real | Piyo of bool -> (real -> int) * real"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Datatype {
            name: Symbol::new("hoge"),
            constructors: vec![
                (Symbol::new("Hoge"), Some(Type::Int)),
                (Symbol::new("Fuga"), Some(Type::Real)),
                (
                    Symbol::new("Piyo"),
                    Some(Type::Fun(
                        Box::new(Type::Datatype(Symbol::new("bool"))),
                        Box::new(Type::Tuple(vec![
                            Type::Fun(Box::new(Type::Real), Box::new(Type::Int)),
                            Type::Real
                        ]))
                    ))
                )
            ]
        },])
    )
}

#[test]
fn parse_datatype_primlike() {
    let input = r#"datatype intlist = Cons of int * intlist | Nil"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Datatype {
            name: Symbol::new("intlist"),
            constructors: vec![
                (
                    Symbol::new("Cons"),
                    Some(Type::Tuple(vec![
                        Type::Int,
                        Type::Datatype(Symbol::new("intlist"))
                    ]))
                ),
                (Symbol::new("Nil"), None)
            ]
        },])
    )
}

#[test]
fn parse_fun_unary() {
    let input = r#"fun f x = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::D(DerivedDeclaration::Fun {
            name: Symbol::new("f"),
            clauses: vec![(
                vec![Pattern {
                    ty: Empty {},
                    span: Location::new(1, 7)..Location::new(1, 8),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x"),
                    }
                }],
                Expr {
                    ty: Empty {},
                    span: Location::new(1, 11)..Location::new(1, 12),
                    inner: ExprKind::Symbol {
                        name: Symbol::new("x"),
                    }
                }
            )]
        }),])
    )
}

#[test]
fn parse_fun_binary() {
    let input = r#"fun f x y = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::D(DerivedDeclaration::Fun {
            name: Symbol::new("f"),
            clauses: vec![(
                vec![
                    Pattern {
                        ty: Empty {},
                        span: Location::new(1, 7)..Location::new(1, 8),
                        inner: PatternKind::Variable {
                            name: Symbol::new("x"),
                        }
                    },
                    Pattern {
                        ty: Empty {},
                        span: Location::new(1, 9)..Location::new(1, 10),
                        inner: PatternKind::Variable {
                            name: Symbol::new("y"),
                        }
                    }
                ],
                Expr {
                    ty: Empty {},
                    span: Location::new(1, 13)..Location::new(1, 14),
                    inner: ExprKind::Symbol {
                        name: Symbol::new("x"),
                    }
                }
            )]
        }),])
    )
}

#[test]
fn parse_fun_pattern() {
    let input = r#"fun f (x, y) = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::D(DerivedDeclaration::Fun {
            name: Symbol::new("f"),
            clauses: vec![(
                vec![Pattern {
                    ty: Empty {},
                    span: Location::new(1, 7)..Location::new(1, 13),
                    inner: PatternKind::Tuple {
                        tuple: vec![
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 8)..Location::new(1, 9),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("x"),
                                }
                            },
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 11)..Location::new(1, 12),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("y"),
                                }
                            },
                        ]
                    }
                }],
                Expr {
                    ty: Empty {},
                    span: Location::new(1, 16)..Location::new(1, 17),
                    inner: ExprKind::Symbol {
                        name: Symbol::new("x"),
                    }
                }
            )]
        }),])
    )
}

#[test]
fn parse_fun_op() {
    let input = r#"fun op+(x, y) = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::D(DerivedDeclaration::Fun {
            name: Symbol::new("+"),
            clauses: vec![(
                vec![Pattern {
                    ty: Empty {},
                    span: Location::new(1, 8)..Location::new(1, 14),
                    inner: PatternKind::Tuple {
                        tuple: vec![
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 9)..Location::new(1, 10),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("x"),
                                }
                            },
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 12)..Location::new(1, 13),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("y"),
                                }
                            },
                        ]
                    }
                }],
                Expr {
                    ty: Empty {},
                    span: Location::new(1, 17)..Location::new(1, 18),
                    inner: ExprKind::Symbol {
                        name: Symbol::new("x"),
                    }
                }
            )]
        }),])
    )
}

#[test]
fn parse_fun_multiclause() {
    let input = r#"fun f Nil _ = Nil | f _ Nil = Nil"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::D(DerivedDeclaration::Fun {
            name: Symbol::new("f"),
            clauses: vec![
                (
                    vec![
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 7)..Location::new(1, 10),
                            inner: PatternKind::Variable {
                                name: Symbol::new("Nil"),
                            }
                        },
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 11)..Location::new(1, 12),
                            inner: PatternKind::Wildcard {}
                        }
                    ],
                    Expr {
                        ty: Empty {},
                        span: Location::new(1, 15)..Location::new(1, 18),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("Nil"),
                        }
                    }
                ),
                (
                    vec![
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 23)..Location::new(1, 24),
                            inner: PatternKind::Wildcard {}
                        },
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 25)..Location::new(1, 28),
                            inner: PatternKind::Variable {
                                name: Symbol::new("Nil"),
                            }
                        },
                    ],
                    Expr {
                        ty: Empty {},
                        span: Location::new(1, 31)..Location::new(1, 34),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("Nil"),
                        }
                    }
                )
            ]
        }),])
    )
}

#[test]
fn parse_fun_multiclause_different_fnname() {
    let input = r#"fun f Nil _ = Nil | g _ Nil = Nil"#;
    let ast = parse(input);
    assert!(ast.is_err())
}

#[test]
fn parse_if() {
    let input = r#"val x = if true then false else true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 37),
                inner: ExprKind::D(DerivedExprKind::If {
                    cond: Expr {
                        ty: Empty {},
                        span: Location::new(1, 12)..Location::new(1, 16),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("true")
                        }
                    }
                    .boxed(),
                    then: Expr {
                        ty: Empty {},
                        span: Location::new(1, 22)..Location::new(1, 27),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("false")
                        }
                    }
                    .boxed(),
                    else_: Expr {
                        ty: Empty {},
                        span: Location::new(1, 33)..Location::new(1, 37),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("true")
                        }
                    }
                    .boxed(),
                })
            },
        },])
    )
}

#[test]
fn parse_case_bool() {
    let input = r#"val x = case true of true => false | false => true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 51),
                inner: ExprKind::Case {
                    cond: Expr {
                        ty: Empty {},
                        span: Location::new(1, 14)..Location::new(1, 18),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("true")
                        }
                    }
                    .boxed(),
                    clauses: vec![
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 22)..Location::new(1, 26),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("true")
                                }
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 30)..Location::new(1, 35),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("false")
                                }
                            },
                        ),
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 38)..Location::new(1, 43),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("false"),
                                }
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 47)..Location::new(1, 51),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("true"),
                                }
                            },
                        ),
                    ],
                }
            },
        },])
    )
}

#[test]
fn parse_case_constructor() {
    let input = r#"val x = case NONE of SOME x => false | NONE => true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 52),
                inner: ExprKind::Case {
                    cond: Expr {
                        ty: Empty {},
                        span: Location::new(1, 14)..Location::new(1, 18),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("NONE")
                        }
                    }
                    .boxed(),
                    clauses: vec![
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 22)..Location::new(1, 28),
                                inner: PatternKind::Constructor {
                                    name: Symbol::new("SOME"),
                                    arg: Some(Box::new(Pattern {
                                        ty: Empty {},
                                        span: Location::new(1, 27)..Location::new(1, 28),
                                        inner: PatternKind::Variable {
                                            name: Symbol::new("x"),
                                        }
                                    })),
                                }
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 32)..Location::new(1, 37),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("false")
                                }
                            },
                        ),
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 40)..Location::new(1, 44),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("NONE"),
                                }
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 48)..Location::new(1, 52),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("true"),
                                }
                            },
                        ),
                    ],
                }
            },
        },])
    )
}

#[test]
fn parse_case_var() {
    let input = r#"val x = case true of true => false | x => true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 47),
                inner: ExprKind::Case {
                    cond: Expr {
                        ty: Empty {},
                        span: Location::new(1, 14)..Location::new(1, 18),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("true")
                        }
                    }
                    .boxed(),
                    clauses: vec![
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 22)..Location::new(1, 26),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("true")
                                }
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 30)..Location::new(1, 35),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("false")
                                }
                            },
                        ),
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 38)..Location::new(1, 39),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("x"),
                                }
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 43)..Location::new(1, 47),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("true")
                                }
                            },
                        ),
                    ],
                }
            },
        },])
    )
}

#[test]
fn parse_case_wildcard() {
    let input = r#"val x = case true of true => false | _ => true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 47),
                inner: ExprKind::Case {
                    cond: Expr {
                        ty: Empty {},
                        span: Location::new(1, 14)..Location::new(1, 18),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("true")
                        }
                    }
                    .boxed(),
                    clauses: vec![
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 22)..Location::new(1, 26),
                                inner: PatternKind::Variable {
                                    name: Symbol::new("true")
                                }
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 30)..Location::new(1, 35),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("false")
                                }
                            },
                        ),
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 38)..Location::new(1, 39),
                                inner: PatternKind::Wildcard {}
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 43)..Location::new(1, 47),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("true")
                                }
                            },
                        ),
                    ],
                }
            },
        },])
    )
}

#[test]
fn parse_case_int() {
    let input = r#"val x = case 3 of 1 => 1 | 2 => 2 | _ => 10"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 44),
                inner: ExprKind::Case {
                    cond: Expr {
                        ty: Empty {},
                        span: Location::new(1, 14)..Location::new(1, 15),
                        inner: ExprKind::Literal {
                            value: Literal::Int(3),
                        }
                    }
                    .boxed(),
                    clauses: vec![
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 19)..Location::new(1, 20),
                                inner: PatternKind::Constant { value: 1 }
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 24)..Location::new(1, 25),
                                inner: ExprKind::Literal {
                                    value: Literal::Int(1),
                                }
                            },
                        ),
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 28)..Location::new(1, 29),
                                inner: PatternKind::Constant { value: 2 }
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 33)..Location::new(1, 34),
                                inner: ExprKind::Literal {
                                    value: Literal::Int(2),
                                }
                            },
                        ),
                        (
                            Pattern {
                                ty: Empty {},
                                span: Location::new(1, 37)..Location::new(1, 38),
                                inner: PatternKind::Wildcard {}
                            },
                            Expr {
                                ty: Empty {},
                                span: Location::new(1, 42)..Location::new(1, 44),
                                inner: ExprKind::Literal {
                                    value: Literal::Int(10),
                                }
                            },
                        ),
                    ],
                }
            },
        },])
    )
}

#[test]
fn parse_case_tuple() {
    let input = r#"val x = case (1, 2, 3) of (x, y, z) => z"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 41),
                inner: ExprKind::Case {
                    cond: Expr {
                        ty: Empty {},
                        span: Location::new(1, 14)..Location::new(1, 23),
                        inner: ExprKind::Tuple {
                            tuple: vec![
                                Expr {
                                    ty: Empty {},
                                    span: Location::new(1, 15)..Location::new(1, 16),
                                    inner: ExprKind::Literal {
                                        value: Literal::Int(1),
                                    }
                                },
                                Expr {
                                    ty: Empty {},
                                    span: Location::new(1, 18)..Location::new(1, 19),
                                    inner: ExprKind::Literal {
                                        value: Literal::Int(2),
                                    }
                                },
                                Expr {
                                    ty: Empty {},
                                    span: Location::new(1, 21)..Location::new(1, 22),
                                    inner: ExprKind::Literal {
                                        value: Literal::Int(3),
                                    }
                                },
                            ],
                        }
                    }
                    .boxed(),
                    clauses: vec![(
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 27)..Location::new(1, 36),
                            inner: PatternKind::Tuple {
                                tuple: vec![
                                    Pattern {
                                        ty: Empty {},
                                        span: Location::new(1, 28)..Location::new(1, 29),
                                        inner: PatternKind::Variable {
                                            name: Symbol::new("x"),
                                        }
                                    },
                                    Pattern {
                                        ty: Empty {},
                                        span: Location::new(1, 31)..Location::new(1, 32),
                                        inner: PatternKind::Variable {
                                            name: Symbol::new("y"),
                                        }
                                    },
                                    Pattern {
                                        ty: Empty {},
                                        span: Location::new(1, 34)..Location::new(1, 35),
                                        inner: PatternKind::Variable {
                                            name: Symbol::new("z"),
                                        }
                                    },
                                ],
                            }
                        },
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 40)..Location::new(1, 41),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("z"),
                            }
                        },
                    ),],
                }
            },
        },])
    )
}

#[test]
fn parse_pattern_unit() {
    let input = r#"val () = ()"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 7),
                inner: PatternKind::Tuple { tuple: vec![] }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 10)..Location::new(1, 12),
                inner: ExprKind::Tuple { tuple: vec![] }
            }
        }])
    )
}

#[test]
fn parse_case_val_pattern_wildcard() {
    let input = r#"val _ = 1"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Wildcard {}
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 10),
                inner: ExprKind::Literal {
                    value: Literal::Int(1),
                }
            },
        },])
    )
}

#[test]
fn parse_funarg_pattern() {
    let input = r#"fun xor (SOME _) (SOME _) = NONE | xor NONE (SOME x) = SOME x | xor (SOME x) NONE = SOME x | xor NONE NONE = NONE"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::D(DerivedDeclaration::Fun {
            name: Symbol::new("xor"),
            clauses: vec![
                (
                    vec![
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 10)..Location::new(1, 16),
                            inner: PatternKind::Constructor {
                                name: Symbol::new("SOME"),
                                arg: Some(Box::new(Pattern {
                                    ty: Empty {},
                                    span: Location::new(1, 15)..Location::new(1, 16),
                                    inner: PatternKind::Wildcard {}
                                }))
                            }
                        },
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 19)..Location::new(1, 25),
                            inner: PatternKind::Constructor {
                                name: Symbol::new("SOME"),
                                arg: Some(Box::new(Pattern {
                                    ty: Empty {},
                                    span: Location::new(1, 24)..Location::new(1, 25),
                                    inner: PatternKind::Wildcard {}
                                }))
                            }
                        }
                    ],
                    Expr {
                        ty: Empty {},
                        span: Location::new(1, 29)..Location::new(1, 33),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("NONE"),
                        }
                    }
                ),
                (
                    vec![
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 40)..Location::new(1, 44),
                            inner: PatternKind::Variable {
                                name: Symbol::new("NONE"),
                            }
                        },
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 46)..Location::new(1, 52),
                            inner: PatternKind::Constructor {
                                name: Symbol::new("SOME"),
                                arg: Some(Box::new(Pattern {
                                    ty: Empty {},
                                    span: Location::new(1, 51)..Location::new(1, 52),
                                    inner: PatternKind::Variable {
                                        name: Symbol::new("x")
                                    }
                                }))
                            }
                        }
                    ],
                    Expr {
                        ty: Empty {},
                        span: Location::new(1, 56)..Location::new(1, 62),
                        inner: ExprKind::App {
                            fun: Expr {
                                ty: Empty {},
                                span: Location::new(1, 56)..Location::new(1, 60),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("SOME")
                                }
                            }
                            .boxed(),
                            arg: Expr {
                                ty: Empty {},
                                span: Location::new(1, 61)..Location::new(1, 62),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("x")
                                }
                            }
                            .boxed(),
                        }
                    }
                ),
                (
                    vec![
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 70)..Location::new(1, 76),
                            inner: PatternKind::Constructor {
                                name: Symbol::new("SOME"),
                                arg: Some(Box::new(Pattern {
                                    ty: Empty {},
                                    span: Location::new(1, 75)..Location::new(1, 76),
                                    inner: PatternKind::Variable {
                                        name: Symbol::new("x")
                                    }
                                }))
                            }
                        },
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 78)..Location::new(1, 82),
                            inner: PatternKind::Variable {
                                name: Symbol::new("NONE"),
                            }
                        },
                    ],
                    Expr {
                        ty: Empty {},
                        span: Location::new(1, 85)..Location::new(1, 91),
                        inner: ExprKind::App {
                            fun: Expr {
                                ty: Empty {},
                                span: Location::new(1, 85)..Location::new(1, 89),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("SOME")
                                }
                            }
                            .boxed(),
                            arg: Expr {
                                ty: Empty {},
                                span: Location::new(1, 90)..Location::new(1, 91),
                                inner: ExprKind::Symbol {
                                    name: Symbol::new("x")
                                }
                            }
                            .boxed(),
                        }
                    }
                ),
                (
                    vec![
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 98)..Location::new(1, 102),
                            inner: PatternKind::Variable {
                                name: Symbol::new("NONE"),
                            }
                        },
                        Pattern {
                            ty: Empty {},
                            span: Location::new(1, 103)..Location::new(1, 107),
                            inner: PatternKind::Variable {
                                name: Symbol::new("NONE"),
                            }
                        },
                    ],
                    Expr {
                        ty: Empty {},
                        span: Location::new(1, 110)..Location::new(1, 114),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("NONE"),
                        }
                    }
                )
            ]
        })])
    )
}

#[test]
fn parse_case_binopr_assoc() {
    let input = r#"infixr 6 :: val x = case Nil of 1::2::3 => ()"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::D(DerivedDeclaration::Infixr {
                priority: Some(6),
                names: vec![Symbol::new("::")],
            }),
            Declaration::Val {
                rec: false,
                pattern: Pattern::new(
                    Location::new(1, 17)..Location::new(1, 18),
                    PatternKind::Variable {
                        name: Symbol::new("x")
                    }
                ),
                expr: Expr::new(
                    Location::new(1, 21)..Location::new(1, 46),
                    ExprKind::Case {
                        cond: Expr::new(
                            Location::new(1, 26)..Location::new(1, 29),
                            ExprKind::Symbol {
                                name: Symbol::new("Nil")
                            }
                        )
                        .boxed(),
                        clauses: vec![(
                            Pattern::new(
                                Location::new(1, 33)..Location::new(1, 40),
                                PatternKind::Constructor {
                                    name: Symbol::new("::"),
                                    arg: Some(
                                        Pattern::new(
                                            Location::new(1, 33)..Location {
                                                line: 1,
                                                column: 40
                                            },
                                            PatternKind::Tuple {
                                                tuple: vec![
                                                    Pattern::new(
                                                        Location::new(1, 33)..Location::new(1, 34),
                                                        PatternKind::Constant { value: 1 }
                                                    ),
                                                    Pattern::new(
                                                        Location::new(1, 36)..Location::new(1, 40),
                                                        PatternKind::Constructor {
                                                            name: Symbol::new("::"),
                                                            arg: Some(
                                                                Pattern::new(
                                                                    Location::new(1, 36)
                                                                        ..Location::new(1, 40),
                                                                    PatternKind::Tuple {
                                                                        tuple: vec![
                                                                        Pattern::new(
                                                                            Location::new(1, 36)
                                                                                ..Location::new(
                                                                                    1, 37
                                                                                ),
                                                                            PatternKind::Constant {
                                                                                value: 2
                                                                            }
                                                                        ),
                                                                        Pattern::new(
                                                                            Location::new(1, 39)
                                                                                ..Location::new(
                                                                                    1, 40
                                                                                ),
                                                                            PatternKind::Constant {
                                                                                value: 3
                                                                            }
                                                                        )
                                                                    ]
                                                                    }
                                                                )
                                                                .boxed()
                                                            )
                                                        }
                                                    )
                                                ]
                                            }
                                        )
                                        .boxed()
                                    )
                                }
                            ),
                            Expr::new(
                                Location::new(1, 44)..Location::new(1, 46),
                                ExprKind::Tuple { tuple: vec![] }
                            )
                        )]
                    }
                )
            }
        ])
    )
}

#[test]
fn parse_multistatement_val_datatype() {
    let input = r#"val version = 1 datatype order = GREATER | EQUAL | LESS"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 5)..Location::new(1, 12),
                    inner: PatternKind::Variable {
                        name: Symbol::new("version")
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 15)..Location::new(1, 16),
                    inner: ExprKind::Literal {
                        value: Literal::Int(1)
                    }
                }
            },
            Declaration::Datatype {
                name: Symbol::new("order"),
                constructors: vec![
                    (Symbol::new("GREATER"), None),
                    (Symbol::new("EQUAL"), None),
                    (Symbol::new("LESS"), None),
                ]
            }
        ])
    )
}

#[test]
fn pares_comment() {
    let input = r#"(* comment (* is *) nestable *)"#;
    let ast = parse(input).unwrap();
    assert_eq!(ast, AST(vec![]));
}

#[test]
fn parse_langitem() {
    let input = r#"__lang_item((bool))__ datatype bool = false | true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::LangItem {
            name: LangItem::Bool,
            decl: Box::new(Declaration::Datatype {
                name: Symbol::new("bool"),
                constructors: vec![(Symbol::new("false"), None), (Symbol::new("true"), None),]
            })
        }])
    )
}

#[test]
fn parse_let() {
    let input = r#"val x = let val y = 1 in y end"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 31),
                inner: ExprKind::Binds {
                    binds: vec![Declaration::Val {
                        rec: false,
                        pattern: Pattern {
                            ty: Empty {},
                            span: Location::new(1, 17)..Location::new(1, 18),
                            inner: PatternKind::Variable {
                                name: Symbol::new("y")
                            }
                        },
                        expr: Expr {
                            ty: Empty {},
                            span: Location::new(1, 21)..Location::new(1, 22),
                            inner: ExprKind::Literal {
                                value: Literal::Int(1)
                            }
                        }
                    }],
                    ret: Expr {
                        ty: Empty {},
                        span: Location::new(1, 26)..Location::new(1, 27),
                        inner: ExprKind::Symbol {
                            name: Symbol::new("y")
                        }
                    }
                    .boxed()
                }
            }
        }])
    )
}

#[test]
fn parse_let_seq() {
    let input = r#"val x = let val y = 1 in y; y end"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Val {
            rec: false,
            pattern: Pattern {
                ty: Empty {},
                span: Location::new(1, 5)..Location::new(1, 6),
                inner: PatternKind::Variable {
                    name: Symbol::new("x"),
                }
            },
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 9)..Location::new(1, 34),
                inner: ExprKind::D(DerivedExprKind::BindSeq {
                    binds: vec![Declaration::Val {
                        rec: false,
                        pattern: Pattern {
                            ty: Empty {},
                            span: Location::new(1, 17)..Location::new(1, 18),
                            inner: PatternKind::Variable {
                                name: Symbol::new("y")
                            }
                        },
                        expr: Expr {
                            ty: Empty {},
                            span: Location::new(1, 21)..Location::new(1, 22),
                            inner: ExprKind::Literal {
                                value: Literal::Int(1)
                            }
                        }
                    }],
                    ret: vec![
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 26)..Location::new(1, 27),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("y")
                            }
                        },
                        Expr {
                            ty: Empty {},
                            span: Location::new(1, 29)..Location::new(1, 30),
                            inner: ExprKind::Symbol {
                                name: Symbol::new("y")
                            }
                        }
                    ]
                })
            }
        }])
    )
}

#[test]
fn parse_toplevel_expr() {
    let input = r#"x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::D(DerivedDeclaration::Expr {
            expr: Expr {
                ty: Empty {},
                span: Location::new(1, 1)..Location::new(1, 2),
                inner: ExprKind::Symbol {
                    name: Symbol::new("x")
                }
            }
        })])
    )
}

#[test]
fn parse_local() {
    let input = r#"local val x = 2 in val y = x end "#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Declaration::Local {
            binds: vec![Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 11)..Location::new(1, 12),
                    inner: PatternKind::Variable {
                        name: Symbol::new("x")
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 15)..Location::new(1, 16),
                    inner: ExprKind::Literal {
                        value: Literal::Int(2)
                    }
                }
            }],
            body: vec![Declaration::Val {
                rec: false,
                pattern: Pattern {
                    ty: Empty {},
                    span: Location::new(1, 24)..Location::new(1, 25),
                    inner: PatternKind::Variable {
                        name: Symbol::new("y")
                    }
                },
                expr: Expr {
                    ty: Empty {},
                    span: Location::new(1, 28)..Location::new(1, 29),
                    inner: ExprKind::Symbol {
                        name: Symbol::new("x")
                    }
                }
            }]
        }])
    )
}
