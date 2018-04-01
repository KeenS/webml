extern crate webml;

use webml::parse;
use webml::prim::*;
use webml::ast::{Expr, TyDefer, Val, AST};

#[test]
fn parse_int() {
    let input = r#"val x = 1"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Val {
                ty: TyDefer::empty(),
                rec: false,
                name: Symbol::new("x"),
                expr: Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Int(1),
                },
            },
        ])
    )
}

#[test]
fn parse_float() {
    let input = r#"val x = 1.0"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Val {
                ty: TyDefer::empty(),
                rec: false,
                name: Symbol::new("x"),
                expr: Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Float(1.0),
                },
            },
        ])
    )
}

#[test]
fn parse_bool_true() {
    let input = r#"val x = true"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Val {
                ty: TyDefer::empty(),
                rec: false,
                name: Symbol::new("x"),
                expr: Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Bool(true),
                },
            },
        ])
    )
}

#[test]
fn parse_bool_false() {
    let input = r#"val x = false"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Val {
                ty: TyDefer::empty(),
                rec: false,
                name: Symbol::new("x"),
                expr: Expr::Lit {
                    ty: TyDefer::empty(),
                    value: Literal::Bool(false),
                },
            },
        ])
    )
}

#[test]
fn parse_fun_unary() {
    let input = r#"fun f x = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Val {
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
            },
        ])
    )
}

#[test]
fn parse_fun_binary() {
    let input = r#"fun f x y = x"#;
    let ast = parse(input).unwrap();
    assert_eq!(
        ast,
        AST(vec![
            Val {
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
            },
        ])
    )
}
