extern crate webml;

use webml::ast::{Expr, Pattern, TyDefer, Val, AST};
use webml::parse;
use webml::prim::*;

#[test]
fn parse_int() {
    let input = r#"val x = 1"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::Lit {
                ty: TyDefer::empty(),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::Lit {
                ty: TyDefer::empty(),
                value: Literal::Float(1.0),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::Lit {
                ty: TyDefer::empty(),
                value: Literal::Bool(true),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::Lit {
                ty: TyDefer::empty(),
                value: Literal::Bool(false),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("f"),
            expr: Expr::Fun {
                param_ty: TyDefer::empty(),
                param: Symbol::new("x"),
                body_ty: TyDefer::empty(),
                body: Box::new(Expr::Sym {
                    ty: TyDefer::empty(),
                    name: Symbol::new("x"),
                }),
            },
        },])
    )
}

#[test]
fn parse_fun_unary() {
    let input = r#"fun f x = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: true,
            name: Symbol::new("f"),
            expr: Expr::Fun {
                param_ty: TyDefer::empty(),
                param: Symbol::new("x"),
                body_ty: TyDefer::empty(),
                body: Box::new(Expr::Sym {
                    ty: TyDefer::empty(),
                    name: Symbol::new("x"),
                }),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: true,
            name: Symbol::new("f"),
            expr: Expr::Fun {
                param_ty: TyDefer::empty(),
                param: Symbol::new("x"),
                body_ty: TyDefer::empty(),
                body: Box::new(Expr::Fun {
                    param_ty: TyDefer::empty(),
                    param: Symbol::new("y"),
                    body_ty: TyDefer::empty(),
                    body: Box::new(Expr::Sym {
                        ty: TyDefer::empty(),
                        name: Symbol::new("x"),
                    }),
                }),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::If {
                ty: TyDefer::empty(),
                cond: Box::new(Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Bool(true),
                }),
                then: Box::new(Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Bool(false),
                }),
                else_: Box::new(Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Bool(true),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::Case {
                ty: TyDefer::empty(),
                cond: Box::new(Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Bool(true),
                }),
                clauses: vec![
                    (
                        Pattern::Lit {
                            value: Literal::Bool(true),
                            ty: TyDefer::empty(),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Bool(false),
                        },
                    ),
                    (
                        Pattern::Lit {
                            value: Literal::Bool(false),
                            ty: TyDefer::empty(),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Bool(true),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::Case {
                ty: TyDefer::empty(),
                cond: Box::new(Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Bool(true),
                }),
                clauses: vec![
                    (
                        Pattern::Lit {
                            value: Literal::Bool(true),
                            ty: TyDefer::empty(),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Bool(false),
                        },
                    ),
                    (
                        Pattern::Var {
                            name: Symbol::new("x"),
                            ty: TyDefer::empty(),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Bool(true),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::Case {
                ty: TyDefer::empty(),
                cond: Box::new(Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Bool(true),
                }),
                clauses: vec![
                    (
                        Pattern::Lit {
                            value: Literal::Bool(true),
                            ty: TyDefer::empty(),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Bool(false),
                        },
                    ),
                    (
                        Pattern::Wildcard {
                            ty: TyDefer::empty(),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Bool(true),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::Case {
                ty: TyDefer::empty(),
                cond: Box::new(Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Int(3),
                }),
                clauses: vec![
                    (
                        Pattern::Lit {
                            value: Literal::Int(1),
                            ty: TyDefer::empty(),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Int(1),
                        },
                    ),
                    (
                        Pattern::Lit {
                            value: Literal::Int(2),
                            ty: TyDefer::empty(),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Int(2),
                        },
                    ),
                    (
                        Pattern::Wildcard {
                            ty: TyDefer::empty(),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
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
        AST(vec![Val {
            ty: TyDefer::empty(),
            rec: false,
            name: Symbol::new("x"),
            expr: Expr::Case {
                ty: TyDefer::empty(),
                cond: Box::new(Expr::Tuple {
                    ty: TyDefer::empty(),
                    tuple: vec![
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Int(1),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Int(2),
                        },
                        Expr::Lit {
                            ty: TyDefer::empty(),
                            value: Literal::Int(3),
                        },
                    ],
                }),
                clauses: vec![(
                    Pattern::Tuple {
                        tuple: vec![
                            (TyDefer::empty(), Symbol::new("x")),
                            (TyDefer::empty(), Symbol::new("y")),
                            (TyDefer::empty(), Symbol::new("z")),
                        ],
                    },
                    Expr::Sym {
                        ty: TyDefer::empty(),
                        name: Symbol::new("z"),
                    },
                ),],
            },
        },])
    )
}
