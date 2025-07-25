use std::env;
use std::io::Read;
use std::rc::Rc;

use std::collections::HashMap;

use clap::{Parser, ValueEnum};

use parser::{Expansion, ExpansionKind, Expr};

use crate::parser::parse;
mod parser;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, ValueEnum, Debug)]
enum Format {
    Raw,
    Bash,
    Fish,
}

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,

    #[arg(long)]
    no_env: bool,

    #[arg(long)]
    no_export: bool,

    #[arg(long)]
    no_dedupe_paths: bool,

    #[arg(value_enum, long, default_value = "raw")]
    format: Format,

    files: Vec<String>,
}

#[derive(Debug, Clone)]
enum AnyStr<'a> {
    Str(&'a str),
    String(String),
}

impl<'a> From<&'a str> for AnyStr<'a> {
    fn from(value: &'a str) -> Self {
        AnyStr::Str(value)
    }
}

impl<'a> From<String> for AnyStr<'a> {
    fn from(value: String) -> Self {
        AnyStr::String(value)
    }
}

type Environ = HashMap<String, Entry>;

#[derive(Debug, Clone)]
struct Entry {
    value: String,
    order: i32,
}

impl Entry {
    fn new(value: String, order: i32) -> Self {
        Self { value, order }
    }
}

fn load_environ(env: &mut Environ) {
    for (key, value) in env::vars() {
        env.insert(key, Entry::new(value.into(), 0));
    }
}

type EvalResult<'a> = Result<AnyStr<'a>, Rc<String>>;

fn eval_expansion<'a>(environ: &mut Environ, exp: Expansion<'a>) -> EvalResult<'a> {
    let value = environ.get(exp.name).and_then(|e| Some(e.value.clone()));

    match exp.kind {
        ExpansionKind::Simple => match value {
            Some(s) => Ok(s.into()),
            None => Ok("".into()),
        },
        ExpansionKind::Default(expr) => match value.filter(|s| !s.is_empty()) {
            // Use expr if unset or empty
            Some(s) => Ok(s.into()),
            None => eval_expr(environ, expr),
        },
        ExpansionKind::DefaultNull(expr) => match value {
            // Use expr if unset
            Some(s) => Ok(s.into()),
            None => eval_expr(environ, expr),
        },
        ExpansionKind::Alt(expr) => match value.filter(|s| !s.is_empty()) {
            // return replacement if value is set and not null
            Some(_) => eval_expr(environ, expr),
            None => Ok("".into()),
        },
        ExpansionKind::AltNull(expr) => match value {
            // return replacement if value is set
            Some(_) => eval_expr(environ, expr),
            None => Ok("".into()),
        },
        ExpansionKind::Err(_) => panic!("Not implemented"),
        ExpansionKind::ErrNull(_) => panic!("Not implemented"),
    }
}

fn eval_expr<'a>(environ: &mut Environ, expr: Expr<'a>) -> EvalResult<'a> {
    match expr {
        Expr::Str(s) => Ok(s.into()),
        Expr::String(s) => Ok(s.into()),
        Expr::Substiution(expansion) => eval_expansion(environ, *expansion),
        Expr::Composite(items) => {
            // Since items.len() >= 2, we must do concatenation
            let mut res = String::new();

            for expr in items.into_iter() {
                match eval_expr(environ, expr)? {
                    AnyStr::Str(s) => res.push_str(s),
                    AnyStr::String(mut s) => {
                        // Swap them if res is empty
                        if res.is_empty() {
                            std::mem::swap(&mut res, &mut s);
                        } else {
                            res.push_str(&s)
                        }
                    }
                }
            }

            Ok(res.into())
        }
    }
}

fn is_path(_args: &Cli, name: &str) -> bool {
    // Other paths:
    // https://access.redhat.com/documentation/zh-tw/red_hat_software_collections/3/html/packaging_guide/sect-commonly_used_path_redefinitions
    match name {
        "PATH" | "MANPATH" | "INFOPATH" => true,
        _ => false,
    }
}

fn dedupe_path(value: &str) -> Option<String> {
    const PATH_SEP: char = ':';

    use std::collections::HashSet;
    let mut work = String::with_capacity(value.len());
    let mut found = HashSet::new();

    // Slow search
    for item in value.split(PATH_SEP) {
        if found.insert(item) {
            if !work.is_empty() {
                work.push(PATH_SEP);
            }

            work.push_str(item);
        }
    }

    if work != value { Some(work) } else { None }
}

fn read_stdin() -> Result<String, std::io::Error> {
    let stdin = std::io::stdin();

    let mut buf = String::new();
    let mut handle = stdin.lock();

    handle.read_to_string(&mut buf).and_then(|_| Ok(buf))
}

fn main() -> Result<(), String> {
    let args = Cli::parse();
    if args.verbose > 0 {
        eprintln!("{args:?}");
    }

    // if args.files.len() == 0 {
    //     return Err("No files were specified.".to_owned());
    // }

    let mut env = Environ::new();

    if !args.no_env {
        load_environ(&mut env);
    }

    let file_contents = {
        let mut res = Vec::with_capacity(args.files.len());

        if args.files.is_empty() {
            if let Ok(buf) = read_stdin() {
                res.push(buf);
            }
        } else {
            for file in &args.files {
                if file == "-" {
                    res.push(read_stdin().unwrap());
                } else {
                    res.push(std::fs::read_to_string(file).unwrap());
                }
            }
        }

        res
    };

    let mut order = 1;
    for data in file_contents.iter() {
        process_dotfile(&args, &mut env, data, &mut order)?;
    }

    if !args.no_dedupe_paths {
        // Dedupe path variables
        for (name, entry) in env.iter_mut() {
            if !is_path(&args, name) || entry.order <= 0 {
                continue;
            }
            if let Some(new_value) = dedupe_path(&entry.value) {
                entry.value = new_value;
            }
        }
    }

    let pairs = {
        // Convert the computed variables to (name, value) pairs sorted by insert order
        // Remove pairs with non-positive `order` value
        let mut res = env
            .iter()
            .filter_map(|(name, entry)| {
                if entry.order > 0 {
                    Some((name.as_str(), entry.value.as_str()))
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        res.sort_by_key(|&(s, _)| {
            env.get(s).map(|e| e.order).unwrap_or(
                order + 1, // the key must exist
            )
        });
        res
    };

    let print_fn = match args.format {
        Format::Raw => print_raw,
        Format::Bash => print_bash,
        Format::Fish => print_fish,
    };

    print_fn(&args, pairs);

    Ok(())
}

fn process_dotfile(
    args: &Cli,
    env: &mut Environ,
    data: &String,
    order: &mut i32,
) -> Result<(), String> {
    let mut input = data.as_str();

    while input.len() > 0 {
        match parse(input) {
            Ok((remaining, option)) => {
                let consumed = input.len() - remaining.len();
                assert!(consumed > 0);

                if args.verbose > 0 {
                    eprintln!("Consumed: {:?} {:?}", &input[0..consumed], option);
                }
                input = remaining;

                match option {
                    None => continue,
                    Some((name, expr)) => {
                        let value = match eval_expr(env, expr) {
                            Ok(AnyStr::Str(s)) => s.to_owned(),
                            Ok(AnyStr::String(s)) => s,
                            Err(e) => return Err(e.as_str().to_owned()),
                        };

                        if args.verbose > 0 {
                            eprintln!("Set {:?} to {:?}", name, value);
                        }

                        // HashMap must take owned key in insert() or entry()
                        // These two methods are the only ways to insert/replace keys
                        // The raw entry API is still in unstable stage.
                        env.insert(name.to_owned(), Entry::new(value, *order));
                        *order += 1;
                    }
                }
            }
            Err(e) => return Err(format!("parse error: {:?}", e)),
        }
    }

    Ok(())
}

fn print_raw(_args: &Cli, pairs: Vec<(&str, &str)>) {
    for (name, value) in pairs {
        println!("{}={}", name, value);
    }
}

fn print_bash(args: &Cli, pairs: Vec<(&str, &str)>) {
    let prefix = if args.no_export { "" } else { "export " };
    let mut work = String::new();

    #[derive(Debug, PartialEq, Eq)]
    enum State {
        UNQUOTED,
        SINGLE,
        DOUBLE,
    }

    for (name, value) in pairs {
        work.clear();
        work.reserve(value.len() + 2); // 2 for quotes

        let mut state = State::UNQUOTED;

        for ch in value.chars() {
            match ch {
                '\'' | '\\' => {
                    match state {
                        State::UNQUOTED => work.push('"'),
                        State::SINGLE => work.push_str("'\""),
                        State::DOUBLE => {}
                    }

                    state = State::DOUBLE;

                    work.extend([
                        '\\', // esc control
                        ch,   // esc char
                    ])
                }
                _ => {
                    match state {
                        State::SINGLE => {}
                        State::UNQUOTED => work.push('\''),
                        State::DOUBLE => work.push_str("\"'"),
                    }

                    state = State::SINGLE;
                    work.push(ch);
                }
            }
        }

        match state {
            State::UNQUOTED => {
                if work.is_empty() {
                    work.push_str("''")
                }
            }
            State::SINGLE => work.push('\''),
            State::DOUBLE => work.push('"'),
        }

        println!("{}{}={}", prefix, name, work);
    }
}

fn print_fish(args: &Cli, pairs: Vec<(&str, &str)>) {
    let scope = if args.no_export { "-g" } else { "-gx" };

    let mut work = String::new();
    let quote = |input: &str, out: &mut String| {
        out.push('\'');
        for ch in input.chars() {
            match ch {
                '\\' | '\'' => {
                    out.push('\\');
                    out.push(ch)
                }
                _ => out.push(ch),
            }
        }
        out.push('\'');
    };

    for (name, value) in pairs {
        work.clear();
        work.reserve(value.len() + 2);

        if is_path(args, name) {
            for (i, item) in value.split(':').enumerate() {
                if i > 0 {
                    work.push(' ');
                }
                quote(item, &mut work);
            }
        } else {
            quote(value, &mut work);
        }

        println!("set {} {} {}", scope, name, work);
    }
}
