use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use chumsky::{Parser, Stream};
use inkwell::context::Context;
use std::{fs::File, io::Read, ops::Range, path::PathBuf, process::exit};

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

    let context = Context::create();
    let module = context.create_module("main");
    let builder = context.create_builder();

    let i32_type = context.i32_type();
    let main_type = i32_type.fn_type(&[], false);
    let function = module.add_function("main", main_type, None);
    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);
    builder.build_return(Some(&i32_type.const_int(0, false)));

    print!("{}", module.print_to_string().to_string());
}
