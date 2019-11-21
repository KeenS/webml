use webml::ast::{Expr, Pattern, Statement, Type, AST};
use webml::parse;
use webml::prim::*;

#[test]
fn parse_int() {
    let input = r#"val x = 1"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::Literal {
                ty: (),
                value: Literal::Int(1),
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
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::Literal {
                ty: (),
                value: Literal::Real(1.0),
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
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::Constructor {
                ty: (),
                arg: None,
                name: Symbol::new("true")
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
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::Constructor {
                ty: (),
                arg: None,
                name: Symbol::new("false")
            },
        },])
    )
}

#[test]
fn parse_fn_unary() {
    let input = r#"val f = fn x => x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("f"),
                ty: (),
            },
            expr: Expr::Fn {
                ty: (),
                param: Symbol::new("x"),
                body: Box::new(Expr::Symbol {
                    ty: (),
                    name: Symbol::new("x"),
                }),
            },
        },])
    )
}

#[test]
fn parse_datatype_single() {
    let input = r#"datatype hoge = Hoge"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Statement::Datatype {
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
        AST(vec![Statement::Datatype {
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
        AST(vec![Statement::Datatype {
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
    let input = r#"datatype hoge = Hoge of int | Fuga of real | Piyo of bool -> real -> int"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Statement::Datatype {
            name: Symbol::new("hoge"),
            constructors: vec![
                (Symbol::new("Hoge"), Some(Type::Int)),
                (Symbol::new("Fuga"), Some(Type::Real)),
                (
                    Symbol::new("Piyo"),
                    Some(Type::Fun(
                        Box::new(Type::Datatype(Symbol::new("bool"))),
                        Box::new(Type::Fun(Box::new(Type::Real), Box::new(Type::Int)))
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
        AST(vec![Statement::Datatype {
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
        AST(vec![Statement::Datatype {
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
fn parse_fun_unary() {
    let input = r#"fun f x = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Statement::Fun {
            name: Symbol::new("f"),
            params: vec![Pattern::Variable {
                name: Symbol::new("x"),
                ty: ()
            }],
            expr: Expr::Symbol {
                ty: (),
                name: Symbol::new("x"),
            },
        },])
    )
}

#[test]
fn parse_fun_binary() {
    let input = r#"fun f x y = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Statement::Fun {
            name: Symbol::new("f"),
            params: vec![
                Pattern::Variable {
                    name: Symbol::new("x"),
                    ty: ()
                },
                Pattern::Variable {
                    name: Symbol::new("y"),
                    ty: ()
                }
            ],
            expr: Expr::Symbol {
                ty: (),
                name: Symbol::new("x"),
            },
        },])
    )
}

#[test]
fn parse_fun_pattern() {
    let input = r#"fun f (x, y) = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Statement::Fun {
            name: Symbol::new("f"),
            params: vec![Pattern::Tuple {
                ty: (),
                tuple: vec![((), Symbol::new("x")), ((), Symbol::new("y")),]
            }],
            expr: Expr::Symbol {
                ty: (),
                name: Symbol::new("x"),
            },
        },])
    )
}

#[test]
fn parse_if() {
    let input = r#"val x = if true then false else true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::If {
                ty: (),
                cond: Box::new(Expr::Constructor {
                    ty: (),
                    arg: None,
                    name: Symbol::new("true")
                }),
                then: Box::new(Expr::Constructor {
                    ty: (),
                    arg: None,
                    name: Symbol::new("false")
                }),
                else_: Box::new(Expr::Constructor {
                    ty: (),
                    arg: None,
                    name: Symbol::new("true")
                }),
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
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::Case {
                ty: (),
                cond: Box::new(Expr::Constructor {
                    ty: (),
                    arg: None,
                    name: Symbol::new("true")
                }),
                clauses: vec![
                    (
                        Pattern::Constructor {
                            ty: (),
                            arg: None,
                            name: Symbol::new("true")
                        },
                        Expr::Constructor {
                            ty: (),
                            arg: None,
                            name: Symbol::new("false")
                        },
                    ),
                    (
                        Pattern::Constructor {
                            ty: (),
                            arg: None,
                            name: Symbol::new("false"),
                        },
                        Expr::Constructor {
                            ty: (),
                            name: Symbol::new("true"),
                            arg: None,
                        },
                    ),
                ],
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
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::Case {
                ty: (),
                cond: Box::new(Expr::Constructor {
                    ty: (),
                    arg: None,
                    name: Symbol::new("true")
                }),
                clauses: vec![
                    (
                        Pattern::Constructor {
                            ty: (),
                            arg: None,
                            name: Symbol::new("true")
                        },
                        Expr::Constructor {
                            ty: (),
                            arg: None,
                            name: Symbol::new("false")
                        },
                    ),
                    (
                        Pattern::Variable {
                            ty: (),
                            name: Symbol::new("x"),
                        },
                        Expr::Constructor {
                            ty: (),
                            arg: None,
                            name: Symbol::new("true")
                        },
                    ),
                ],
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
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::Case {
                ty: (),
                cond: Box::new(Expr::Constructor {
                    ty: (),
                    arg: None,
                    name: Symbol::new("true")
                }),
                clauses: vec![
                    (
                        Pattern::Constructor {
                            ty: (),
                            arg: None,
                            name: Symbol::new("true")
                        },
                        Expr::Constructor {
                            ty: (),
                            arg: None,
                            name: Symbol::new("false")
                        },
                    ),
                    (
                        Pattern::Wildcard { ty: () },
                        Expr::Constructor {
                            ty: (),
                            arg: None,
                            name: Symbol::new("true")
                        },
                    ),
                ],
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
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::Case {
                ty: (),
                cond: Box::new(Expr::Literal {
                    ty: (),
                    value: Literal::Int(3),
                }),
                clauses: vec![
                    (
                        Pattern::Constant { value: 1, ty: () },
                        Expr::Literal {
                            ty: (),
                            value: Literal::Int(1),
                        },
                    ),
                    (
                        Pattern::Constant { value: 2, ty: () },
                        Expr::Literal {
                            ty: (),
                            value: Literal::Int(2),
                        },
                    ),
                    (
                        Pattern::Wildcard { ty: () },
                        Expr::Literal {
                            ty: (),
                            value: Literal::Int(10),
                        },
                    ),
                ],
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
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Variable {
                name: Symbol::new("x"),
                ty: (),
            },
            expr: Expr::Case {
                ty: (),
                cond: Box::new(Expr::Tuple {
                    ty: (),
                    tuple: vec![
                        Expr::Literal {
                            ty: (),
                            value: Literal::Int(1),
                        },
                        Expr::Literal {
                            ty: (),
                            value: Literal::Int(2),
                        },
                        Expr::Literal {
                            ty: (),
                            value: Literal::Int(3),
                        },
                    ],
                }),
                clauses: vec![(
                    Pattern::Tuple {
                        tuple: vec![
                            ((), Symbol::new("x")),
                            ((), Symbol::new("y")),
                            ((), Symbol::new("z")),
                        ],
                        ty: ()
                    },
                    Expr::Symbol {
                        ty: (),
                        name: Symbol::new("z"),
                    },
                ),],
            },
        },])
    )
}

#[test]
fn parse_case_val_pattern_wildcard() {
    let input = r#"val _ = 1"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Statement::Val {
            rec: false,
            pattern: Pattern::Wildcard { ty: () },
            expr: Expr::Literal {
                ty: (),
                value: Literal::Int(1),
            },
        },])
    )
}
