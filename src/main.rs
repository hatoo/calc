use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{Parser, Stream};
use inkwell::context::Context;
use std::{fs::File, io::Read, ops::Range, path::PathBuf, process::exit};

mod codegen;
mod parser;
mod token;

type Span = Range<usize>;

#[derive(clap::Parser, Debug)]
struct Args {
    input_path: PathBuf,
}

fn main() {
    let args = <Args as clap::Parser>::parse();

    let mut src = String::new();
    let path = args.input_path.as_path();
    let src_id = path.to_string_lossy();

    File::open(path)
        .expect("load src")
        .read_to_string(&mut src)
        .expect("read to string");

    let (tokens, errs) = token::lexer().parse_recovery(src.as_str());

    if !errs.is_empty() {
        errs.into_iter().for_each(|e| {
            let msg = match e.reason() {
                chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
                chumsky::error::SimpleReason::Unexpected => {
                    if let Some(c) = e.found() {
                        format!("unexpected char '{}'", c.fg(Color::Red))
                    } else {
                        format!("unexpected char")
                    }
                }
                _ => format!("unknown Error"),
            };

            Report::build(ReportKind::Error, &src_id, e.span().start)
                .with_message("lexer error")
                .with_label(Label::new((&src_id, e.span())).with_message(msg))
                .finish()
                .print((&src_id, Source::from(&src)))
                .expect("print error");
        });

        exit(1);
    }

    let tokens = tokens.unwrap();

    let len = src.chars().count();
    let (ast, errs) =
        parser::parse_ast().parse_recovery(Stream::from_iter(len..len, tokens.into_iter()));

    if !errs.is_empty() {
        errs.into_iter().for_each(|e| {
            let msg = match e.reason() {
                chumsky::error::SimpleReason::Custom(msg) => msg.clone(),
                chumsky::error::SimpleReason::Unclosed { .. } => format!("unclosed delimiter"),
                chumsky::error::SimpleReason::Unexpected => format!(
                    "unexpected {}",
                    e.found()
                        .map(|c| format!("token {}", format!("{:?}", c).fg(Color::Red)))
                        .unwrap_or_else(|| "end of input".to_string())
                ),
            };

            Report::build(ReportKind::Error, &src_id, e.span().start)
                .with_message("parser error")
                .with_label(Label::new((&src_id, e.span())).with_message(msg))
                .finish()
                .print((&src_id, Source::from(&src)))
                .expect("print error");
        });

        exit(1);
    }

    let ast = ast.unwrap();

    let context = Context::create();
    let module = context.create_module("main");

    let mut codegen = codegen::Codegen::new(&context, module);
    if let Err(e) = codegen.run(&ast) {
        let msg = match &e {
            codegen::Error::AlreadyDeclared(ident) => {
                format!(
                    "{} is already declared",
                    ident.ident.as_str().fg(Color::Red)
                )
            }
            codegen::Error::Undeclared(ident) => {
                format!("{} is not declared", ident.ident.as_str().fg(Color::Red))
            }
        };

        Report::build(ReportKind::Error, &src_id, e.span().start)
            .with_message("codegen error")
            .with_label(Label::new((&src_id, e.span())).with_message(msg))
            .finish()
            .print((&src_id, Source::from(&src)))
            .expect("print error");
        exit(1);
    }

    print!("{}", codegen.module.print_to_string().to_string());
}
