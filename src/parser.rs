use crate::token::Token;
use crate::Span;
use chumsky::prelude::*;

type Error = Simple<Token>;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Operator {
    Plus,
    Minus,
    Mul,
    Div,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
    Ident(String),
    Number(u32),
    BinaryOp {
        op: Operator,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct WithDecl {
    pub vars: Vec<String>,
    pub expr: Expr,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum AST {
    Expr(Expr),
    WithDecl(WithDecl),
}

pub fn parse_ast() -> impl Parser<Token, AST, Error = Error> {
    parse_with_decl()
        .map(AST::WithDecl)
        .or(parse_expr().map(AST::Expr))
}

fn parse_with_decl() -> impl Parser<Token, WithDecl, Error = Error> {
    let ident = select! {
        Token::Ident(i) => i
    };

    just(Token::With)
        .ignore_then(
            ident
                .clone()
                .chain(just(Token::Comma).ignore_then(ident.clone()))
                .collect::<Vec<String>>(),
        )
        .then_ignore(just(Token::Colon))
        .then(parse_expr())
        .map(|(vars, expr)| WithDecl { vars, expr })
}

fn parse_expr() -> impl Parser<Token, Expr, Error = Error> {
    recursive(|expr| {
        let factor = select! {
            Token::Number(x) => Expr::Number(x),
            Token::Ident(i) => Expr::Ident(i)
        }
        .or(expr.delimited_by(just(Token::LParen), just(Token::RParen)));

        let op = just(Token::Star)
            .to(Operator::Mul)
            .or(just(Token::Slash).to(Operator::Div));

        let term = factor
            .clone()
            .then(op.then(factor).repeated())
            .foldl(|a, (op, b)| Expr::BinaryOp {
                op,
                left: Box::new(a),
                right: Box::new(b),
            });

        let op = just(Token::Plus)
            .to(Operator::Plus)
            .or(just(Token::Minus).to(Operator::Minus));

        term.clone()
            .then(op.then(term).repeated())
            .foldl(|a, (op, b)| Expr::BinaryOp {
                op,
                left: Box::new(a),
                right: Box::new(b),
            })
    })
}

#[test]
fn test_parse_expr() {
    let tokens = vec![Token::Number(1)];
    assert_eq!(parse_expr().parse(tokens).unwrap(), Expr::Number(1));

    let tokens = vec![Token::Number(1), Token::Plus, Token::Number(2)];
    assert_eq!(
        parse_expr().parse(tokens.as_slice()).unwrap(),
        Expr::BinaryOp {
            op: Operator::Plus,
            left: Box::new(Expr::Number(1)),
            right: Box::new(Expr::Number(2))
        }
    );
}
