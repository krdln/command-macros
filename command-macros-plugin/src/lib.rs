#![feature(proc_macro_span, proc_macro_diagnostic, proc_macro_def_site, external_doc)]

#![doc(include = "../README.md")]

extern crate proc_macro;
extern crate itertools;

use itertools::Itertools;

mod syntax;

use proc_macro::{
    TokenStream,
    TokenTree,
    Span,
    Group,
    Delimiter,
    Literal,
    Spacing,
};

use self::syntax::{
    Expr,
    Pat,
    Stmt,
    from_source,
    new_ident,
    new_spanned_ident,
    new_block,
    surround,
};

use std::iter::once;
use std::iter::FromIterator;
use std::collections::VecDeque;

type Result<T> = ::std::result::Result<T, ()>;

/// Full-featured macro for creating `Command`
///
/// Please read the syntax description in the crate's [documentation](index.html).
///
/// This macro is intended to be used via reexport in `command_macros`
/// crate with a "nightly" feature enabled.
///
/// # Examples
///
/// ```
/// #![feature(proc_macro_hygiene)]
///
/// // also reexported in extern crate command_macros;
/// extern crate command_macros_plugin;
/// 
/// // also reexported as command_macros::command
/// use command_macros_plugin::command;
///
/// fn main() {
///     command!(echo foo --bar ((2+2))=4).status().unwrap();
///     // should echo: foo --bar 4=4
/// }
/// ```
///
/// # Stability
///
/// This is an experimental, nightly-only version of `cmd!` macro,
/// so it might break with a nightly update from time to time.
/// However, it uses a new `proc_macro` interface rather than
/// compiler internals, so the breakage shouldn't occur too often.
///
/// In future, when the `proc_macro` interface is stabilized,
/// this macro should work on stable without significant changes.
#[proc_macro]
pub fn command(input: TokenStream) -> TokenStream {
    match try_expand_command(input) {
        Ok(stream) => stream,
        Err(())    => "::std::process::Command::new(\"dummy\")".parse().unwrap(),
    }
}

fn try_expand_command(input: TokenStream) -> Result<TokenStream> {
    let trees = Parser::new(input).parse()?;
    Ok(generate(trees)?.into_stream())
}

// Data -----------------------------------------------------------------------

#[derive(Debug)]
enum Condition {
    Bool(Expr),
    IfLet(TokenTree, Pat, TokenTree, Expr),
}

#[derive(Debug)]
struct Spanned<T> {
    elem: T,
    span: Span,
}

#[allow(non_snake_case)]
fn Spanned<T>(elem: T, span: Span) -> Spanned<T> {
    Spanned { elem, span }
}

#[derive(Debug)]
struct Block(Spanned<Vec<Tree>>);

#[derive(Debug)]
enum Splice {
    Word(String), // x
    Literal(Literal), // 'x', "x"
    ToStr(Expr), // (x)
    AsOsStr(Expr), // ((x))
}

#[derive(Debug)]
enum Arg {
    Single(Spanned<Splice>),
    Touching(Vec<Spanned<Splice>>),
}

#[derive(Debug)]
struct For {
    for_span: Span,
    pat: Pat,
    in_tt: TokenTree,
    expr: Expr,
    block: Block,
}

#[derive(Debug)]
struct If {
    if_span: Span,
    cond: Condition,
    then_block: Block,
    else_block: Block,
}

type Arm = (Pat, (TokenTree, TokenTree), Block);

#[derive(Debug)]
struct Match {
    match_span: Span,
    expr: Expr,
    block_span: Span,
    arms: Vec<Arm>,
}

#[derive(Debug)]
enum Tree {
    Arg(Arg),
    Args(Spanned<Expr>), // [x]
    Cmd(Expr), // {x}
    If(If),
    Match(Match),
    For(For),
}

impl Arg {
    fn span(&self) -> Span {
        match self {
            Arg::Single(splice) => splice.span,
            Arg::Touching(splices) => {
                splices.first().unwrap().span.join(
                    splices.last().unwrap().span
                ).unwrap()
            }
        }
    }

    fn into_vec(self) -> Vec<Spanned<Splice>> {
        match self {
            Arg::Single(splice) => vec![splice],
            Arg::Touching(splices) => splices,
        }
    }
}

impl Tree {
    fn span(&self) -> Span {
        match self {
            Tree::Arg(arg) => arg.span(),
            Tree::Args(args) => args.span,
            Tree::Cmd(expr) => expr.span(),
            Tree::If(if_) => if_.span(),
            Tree::Match(match_) => match_.span(),
            Tree::For(for_) => for_.span(),
        }
    }
}

impl From<Arg> for Tree {
    fn from(arg: Arg) -> Tree {
        Tree::Arg(arg)
    }
}

impl From<Spanned<Splice>> for Arg {
    fn from(splice: Spanned<Splice>) -> Arg {
        Arg::Single(splice)
    }
}

impl From<Spanned<Splice>> for Tree {
    fn from(splice: Spanned<Splice>) -> Tree {
        Arg::from(splice).into()
    }
}

impl Block {
    fn new(trees: Vec<Tree>, span: Span) -> Block { Block(Spanned(trees, span)) }
}

impl If {
    fn span(&self) -> Span {
        let last_span = if self.else_block.0.elem.is_empty() {
            self.then_block.0.span
        } else {
            self.else_block.0.span
        };
        self.if_span.join(last_span).unwrap_or(self.if_span)
    }
}

impl For {
    fn span(&self) -> Span {
        self.for_span.join(self.block.0.span).unwrap_or(self.for_span)
    }
}

impl Match {
    fn span(&self) -> Span {
        self.match_span.join(self.block_span).unwrap_or(self.match_span)
    }
}

// Generation -----------------------------------------------------------------

fn generate(mut trees: Vec<Tree>) -> Result<Expr> {
    if trees.is_empty() {
        Span::call_site().error("This macro needs at least the command name").emit();
        return Err(());
    }

    let cmd_tree = trees.remove(0);
    let cmd_expr: Expr = match cmd_tree {
        Tree::Arg(arg) => {
            let span = arg.span();
            let str_expr = generate_os_str(arg)?;
            Expr::call(from_source("::std::process::Command::new", span), str_expr, span)
        }
        Tree::Cmd(cmd) => cmd,
        other => {
            other.span().error("Command name should be `cmd` `(cmd_name_expr)` or `{Command_expr}`").emit();
            return Err(())
        }
    };

    let cmd_var: TokenTree = new_ident("cmd");

    let init_stmt = Stmt::new_let(&cmd_var, cmd_expr);
    let mut stmts: Vec<Stmt> = vec![init_stmt];
    stmts.extend(generate_stmts(&cmd_var, trees)?);

    let block = Expr::block(stmts, Expr::from_tt(cmd_var), Span::call_site());

    Ok(block)
}

fn generate_stmts(cmd_var: &TokenTree, trees: Vec<Tree>) -> Result<Vec<Stmt>> {
    trees
        .into_iter()
        .map(|tree| {
            match tree {
                Tree::Arg(arg) => generate_arg(cmd_var, arg),
                Tree::Args(args) => generate_args(cmd_var, args),
                Tree::For(pieces) => generate_for(cmd_var, pieces),
                Tree::If(pieces) => generate_if(cmd_var, pieces),
                Tree::Match(pieces) => generate_match(cmd_var, pieces),
                Tree::Cmd(expr) => {
                    expr.span()
                        .error("Command block could be used only at the beginning")
                        .emit();
                    return Err(())
                }
            }
        })
        .collect()
}

fn generate_block(cmd_var: &TokenTree, block: Block) -> Result<TokenTree> {
    let stmts = generate_stmts(cmd_var, block.0.elem)?;
    Ok(new_block(stmts, block.0.span))
}

fn generate_arg(cmd: &TokenTree, arg: Arg) -> Result<Stmt> {
    let span = arg.span();
    let os_str = generate_os_str(arg)?;
    let call_expr = Expr::call_method_on(cmd, "arg", os_str, span);
    Ok(call_expr.into_stmt())
}

fn generate_args(cmd: &TokenTree, Spanned { elem: expr, span }: Spanned<Expr>) -> Result<Stmt> {
    let call_expr = Expr::call_method_on(cmd, "args", expr, span);
    Ok(call_expr.into_stmt())
}

fn generate_for(cmd_var: &TokenTree, For { for_span, pat, in_tt, expr, block }: For) -> Result<Stmt> {
    let stream = once(new_spanned_ident("for", for_span))
        .chain(pat.0)
        .chain(once(in_tt))
        .chain(expr.into_stream())
        .chain(once(generate_block(cmd_var, block)?))
        .collect();
    Ok(Stmt::from_stream(stream))
}

fn generate_if(cmd_var: &TokenTree, If { if_span, cond, then_block, else_block }: If) -> Result<Stmt> {
    let cond_stream = match cond {
        Condition::Bool(expr)                          => expr.into_stream(),
        Condition::IfLet(let_tt, pat, equals_tt, expr) => {
            once(let_tt)
                .chain(pat.0)
                .chain(once(equals_tt))
                .chain(expr.into_stream())
                .collect()
        }
    };
    let stream = once(new_spanned_ident("if", if_span))
        .chain(cond_stream)
        .chain(once(generate_block(cmd_var, then_block)?))
        .chain(once(new_spanned_ident("else", Span::call_site())))
        .chain(once(generate_block(cmd_var, else_block)?))
        .collect();
    Ok(Stmt::from_stream(stream))
}

fn generate_match(cmd_var: &TokenTree, Match { match_span, expr, block_span, arms }: Match) -> Result<Stmt> {
    let mut arm_stream = Vec::new();
    for arm in arms {
        arm_stream.extend(generate_arm(cmd_var, arm)?);
    }
    let arm_stream = arm_stream.into_iter().collect();

    let block = surround(arm_stream, Delimiter::Brace, block_span);

    let stream = once(new_spanned_ident("match", match_span))
        .chain(expr.into_stream())
        .chain(once(block))
        .collect();

    Ok(Stmt::from_stream(stream))
}

fn generate_arm(cmd_var: &TokenTree, (pat, arrows, block): Arm) -> Result<impl Iterator<Item=TokenTree>> {
    Ok(
        pat.0.into_iter()
            .chain(once(arrows.0))
            .chain(once(arrows.1))
            .chain(once(generate_block(cmd_var, block)?))
    )
}

fn generate_splice(Spanned { elem: splice, span }: Spanned<Splice>) -> Result<Expr> {
    let expr = match splice {
        Splice::Word(word) => Expr::string_literal(&word),
        Splice::Literal(lit) => generate_literal(lit)?,
        Splice::AsOsStr(expr) => Expr::reference(expr, span),
        Splice::ToStr(expr) => Expr::call(
            from_source("ToString::to_string", span),
            Expr::reference(expr, span),
            span
        ),
    };
    Ok(expr)
}

fn generate_literal(literal: Literal) -> Result<Expr> {
    let repr = literal.to_string();
    if repr.starts_with("'") {
        literal.span().error("Use string literals instead").emit();
        Ok(Expr::string_literal("<error>"))
    } else if repr.contains("\"") {
        Ok(Expr::from_tt(literal.into()))
    } else if repr.contains("'") {
        literal.span().error("Unsupported literal").emit();
        Ok(Expr::string_literal("<error>"))
    } else {
        Ok(Expr::string_literal(&literal.to_string()))
    }
}

fn generate_os_str(arg: Arg) -> Result<Expr> {
    let full_span = arg.span();
    match arg {
        Arg::Single(splice) => generate_splice(splice),
        Arg::Touching(splices) => {
            let os_string = Expr::from_source("::std::ffi::OsString::new()", full_span);
            let buf_var = new_ident("buf");
            let init_stmt = Stmt::new_let(&buf_var, os_string);
            let mut stmts = vec![init_stmt];

            for splice in splices {
                let span = splice.span;
                stmts.push(Expr::call_method_on(
                    &buf_var,
                    "push",
                    Expr::reference(generate_splice(splice)?, span),
                    span,
                ).into_stmt())
            }

            Ok(Expr::block(stmts, Expr::from_tt(buf_var), full_span))
        }
    }
}

// Parsing --------------------------------------------------------------------

#[derive(Debug)]
struct Parser {
    last_span: Option<Span>,
    stream: VecDeque<TokenTree>,
}

impl Parser {
    pub fn new(stream: TokenStream) -> Self {
        Parser {
            stream: VecDeque::from_iter(stream),
            last_span: None
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Tree>> {
        let trees = self.parse_trees()?;
        Ok(join_touching(trees))
    }
}

fn join_touching(trees: Vec<Tree>) -> Vec<Tree> {
    trees.into_iter()
        .coalesce(|left, right| {
            match (left, right) {
                (Tree::Arg(left), Tree::Arg(right)) => {
                    if are_separated_spans(left.span(), right.span()) {
                        Err((left.into(), right.into()))
                    } else {
                        let mut splices = left.into_vec();
                        let right = match right {
                            Arg::Single(s) => s,
                            _              => unreachable!(),
                        };
                        splices.push(right);
                        Ok(Arg::Touching(splices).into())
                    }
                }
                (left, right) => Err((left, right))
            }
        })
        .collect()
}

impl Parser {
    fn peek(&self) -> Option<&TokenTree> {
        self.stream.front()
    }

    fn peek_two(&self) -> Option<(&TokenTree, &TokenTree)> {
        match (self.stream.get(0), self.stream.get(1)) {
            (Some(first), Some(second)) => Some((first, second)),
            _ => None
        }
    }

    fn next(&mut self) -> Option<TokenTree> {
        self.stream.pop_front().map(|tt| {
            self.last_span = Some(tt.span());
            tt
        })
    }

    fn parse_block(&mut self) -> Result<Block> {
        let last_span = self.last_span;
        let orig_block = match self.next() {
            Some(TokenTree::Group(group)) => group,
            _ => {
                last_span.unwrap_or(Span::call_site())
                    .error("Expected a {} block after this token")
                    .emit();
                return Err(())
            }
        };
        let trees = Parser::new(orig_block.stream()).parse()?;
        Ok(Block::new(trees, orig_block.span()))
    }

    fn parse_trees(&mut self) -> Result<Vec<Tree>> {
        let mut trees = Vec::new();
        while !self.stream.is_empty() {
            trees.push(self.parse_tree()?);
        }
        Ok(trees)
    }

    fn parse_tree(&mut self) -> Result<Tree> {
        let previous_span = self.last_span;
        let tt = self.next().unwrap();
        let span = tt.span();
        match tt {
            TokenTree::Group(group) => Ok(Parser::parse_splice(group)?.into()),

            TokenTree::Ident(ident) => {
                let next_span = self.peek().map(|next| next.span());

                let word = ident.to_string();
                let warn_keyword = || {
                    if !is_separated_span(previous_span, span, next_span) {
                        span.warning(format!("Keyword `{}` not separated by whitespace", word))
                            .note("Keywords should be separated by whitespace to avoid confusion")
                            .help("To interpret as a string, surround it with quotes")
                            .emit();
                    }
                };

                match word.as_str() {
                    "let" => {
                        warn_keyword();
                        span.error("Let statements are not supported")
                            .note("You can emulate them with `match`")
                            .emit();
                        Err(())
                    }
                    "if" => {
                        warn_keyword();
                        self.parse_if(span)
                    }
                    "for" => {
                        warn_keyword();
                        self.parse_for(span)
                    }
                    "match" => {
                        warn_keyword();
                        self.parse_match(span)
                    }
                    word => {
                        Ok(Spanned(Splice::Word(word.into()), span).into())
                    }
                }
            }

            TokenTree::Literal(lit) => Ok(Spanned(Splice::Literal(lit), span).into()),

            TokenTree::Punct(punct) => {
                match punct.as_char() {
                    '$' => {
                        punct.span()
                            .error("Dollar sign interpollation is not supported")
                            .help("To insert a variable, use `(var)` or `((var))`")
                            .emit();
                    }
                    '>' | '<' => {
                        punct.span().error("File redirection is not supported").emit();
                    }
                    '|' => {
                        punct.span().error("Pipe redirection is not supported").emit();
                    }
                    '&' => {
                        punct.span().error("The `&` and `&&` operators are not supported").emit();
                    }
                    ';' => {
                        punct.span()
                            .error("Unexpected semicolon")
                            .help("To interpret literally, surround in quotes")
                            .note("Semicolon is not needed in this macro")
                            .emit();
                    }
                    ch => {
                        return Ok(Spanned(Splice::Word(ch.to_string()), span).into());
                    }
                }
                Err(())
            }
        }
    }

    fn parse_splice(mut group: Group) -> Result<Tree> {
        let span = group.span();
        let stream = group.stream();

        let tree = match group.delimiter() {
            Delimiter::Brace if is_really_empty(&group) => {
                return Ok(Spanned(Splice::Word("{}".into()), span).into())
            }
            Delimiter::Brace => Tree::Cmd(Expr::from_stream(stream)),
            Delimiter::Parenthesis => {
                match try_into_singleton(&stream) {
                    Some(TokenTree::Group(ref inner))
                        if inner.delimiter() == Delimiter::Parenthesis
                            => {
                                // The clone needed because of pattern guard :(
                                group = inner.clone();
                                Spanned(Splice::ToStr(Expr::from_stream(group.stream())), span).into()
                            }
                    _       => Spanned(Splice::AsOsStr(Expr::from_stream(stream)), span).into(),
                }
            }
            Delimiter::Bracket => Tree::Args(Spanned(Expr::from_stream(stream), span)),
            Delimiter::None => {
                span.error("You've probably tried to use a nested macro.\
                           This is not supported").emit();
                return Err(())
            }
        };

        if group.stream().is_empty() {
            group.span().error("Rust expression expected inside this block").emit();
            return Err(())
        }

        Ok(tree)
    }

    fn parse_if(&mut self, if_span: Span) -> Result<Tree> {
        let cond = if self.is_ident_next("let") {
            let let_tt = self.next().unwrap();
            let pat = Pat(self.parse_until(
                |parser| match parser.peek() {
                    Some(TokenTree::Punct(punct))
                        if punct.as_char() == '=' && punct.spacing() == Spacing::Alone => true,
                    _ => false,
                },
                "`=`",
                "a pattern",
            )?);
            let equals_tt = self.next().unwrap();
            let expr = self.parse_until_block()?;
            Condition::IfLet(let_tt, pat, equals_tt, expr)
        } else {
            Condition::Bool(self.parse_until_block()?)
        };
        let then_block = self.parse_block()?;

        let else_block = if self.is_ident_next("else") {
            let _ = self.next().unwrap();
            if self.is_block_next() {
                self.parse_block()?
            } else if self.is_ident_next("if") {
                let if_tt = self.next().unwrap();
                let inner_if = self.parse_if(if_tt.span())?;
                let inner_span = inner_if.span();
                Block::new(vec![inner_if], inner_span).into()
            } else {
                self.last_span.unwrap()
                    .error("Expected `if` or {} block after this `else`")
                    .emit();
                return Err(());
            }
        } else {
            Block::new(Vec::new(), Span::def_site())
        };

        Ok(Tree::If(If { if_span, cond, then_block, else_block }))
    }

    fn parse_for(&mut self, for_span: Span) -> Result<Tree> {
        let pat = Pat(self.parse_until_ident("in", "a pattern")?);
        let in_tt = self.next().unwrap();
        let expr = self.parse_until_block()?;
        let block = self.parse_block()?;
        Ok(Tree::For(For { for_span, pat, in_tt, expr, block }))
    }

    fn parse_match(&mut self, match_span: Span) -> Result<Tree> {
        use self::Spacing::{Alone, Joint};

        let expr = self.parse_until_block()?;

        let block = match self.next() {
            Some(TokenTree::Group(group)) => group,
            _ => unreachable!(),
        };
        let block_span = block.span();

        // Prevent accidental use of outer parser.
        let _dont_use_self = self;
        let mut parser = Parser::new(block.stream());
        let mut arms = Vec::new();

        while !parser.stream.is_empty() {
            let pat = Pat(parser.parse_until(
                    |parser| match parser.peek_two() {
                        Some((TokenTree::Punct(left), TokenTree::Punct(right)))
                            if left.as_char() == '=' && right.as_char() == '>'
                                && left.spacing() == Joint && right.spacing() == Alone => true,
                                    _ => false,
                    },
                    "`=>`",
                    "a pattern",
                    )?);
            let arrow = (
                parser.next().unwrap(),
                parser.next().unwrap(),
            );
            let block = parser.parse_block()?;
            arms.push((pat, arrow, block))
        }

        drop(_dont_use_self);

        Ok(Tree::Match(Match { match_span, expr, block_span, arms }))
    }

    fn parse_until_block(&mut self) -> Result<Expr> {
        if self.is_block_next() {
            return Ok( Expr::from_tt(self.next().unwrap()) )
        }
        let stream = self.parse_until(Parser::is_block_next, "{} block", "an expression")?;
        Ok(Expr::from_stream(stream))
    }

    fn is_block_next(&self) -> bool {
        match self.peek() {
            Some(&TokenTree::Group(ref group)) if group.delimiter() == Delimiter::Brace => true,
            _ => false,
        }
    }

    fn is_ident_next(&self, expected: &str) -> bool {
        match self.peek() {
            Some(TokenTree::Ident(actual)) if actual.to_string() == expected => true,
            _ => false,
        }
    }

    fn parse_until_ident(&mut self, until_ident: &str, what: &str) -> Result<TokenStream> {
        self.parse_until(
            |parser| parser.is_ident_next(until_ident),
            &format!("`{}`", until_ident),
            what
        )
    }

    fn parse_until<F>(&mut self, until: F, until_str: &str, what: &str) -> Result<TokenStream>
    where F: Fn(&Parser) -> bool
    {
        let mut tts = Vec::new();

        while !until(self) {
            match self.next() {
                Some(tt) => tts.push(tt),
                None => {
                    self.last_span.unwrap()
                        .error(format!("Found end of macro when looking for {}", until_str))
                        .emit();
                    return Err(())
                }
            }
        }

        if tts.is_empty() {
            self.peek().unwrap().span()
                .error(format!("Expected {} before {}", what, until_str))
                .emit();
            return Err(())
        }

        Ok(tts.into_iter().collect())
    }
}

fn are_separated_spans(left: Span, right: Span) -> bool {
    left.end().line != right.start().line ||
    left.end().column < right.start().column
}

fn is_separated_span(left: Option<Span>, this: Span, right: Option<Span>) -> bool {
    left.map_or(true, |left| are_separated_spans(left, this)) &&
    right.map_or(true, |right| are_separated_spans(this, right))
}

fn is_really_empty(group: &Group) -> bool {
    let span = group.span();
    let start = span.start();
    let end = span.end();
    group.stream().is_empty() && start.line == end.line && start.column + 2 == end.column
}

fn try_into_singleton(stream: &TokenStream) -> Option<TokenTree> {
    let mut stream = stream.clone().into_iter();
    let tt = stream.next()?;
    match stream.next() {
        None => Some(tt),
        _    => None
    }
}
