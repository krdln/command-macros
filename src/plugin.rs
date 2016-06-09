use syntax::ptr::P;
use syntax::codemap::{Span, Spanned, respan, DUMMY_SP};
use syntax::parse::token::{self, Lit, Token, DelimToken};
use syntax::parse::token::keywords;
use syntax::parse::token::{intern, intern_and_get_ident};
use syntax::parse::parser::{self, Restrictions};
use syntax::ast::{TokenTree, LitKind, Expr, Stmt, Block, Pat, Ident};
use syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacEager};
use syntax::ext::build::AstBuilder;  // trait for expr_usize

pub fn expand_command(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
    -> Box<MacResult + 'static>
{
    let trees = match Parser::new(cx, args).parse_trees() {
        Ok(t) => t,
        Err(()) => return DummyResult::expr(sp)
    };

    match generate(cx, sp, trees) {
        Ok(e) => MacEager::expr(cx.expr_block(e)),
        Err(()) => DummyResult::expr(sp),
    }
}

enum Condition {
    Bool(P<Expr>),
    IfLet(P<Pat>, P<Expr>),
}

fn span_from_to(from: Span, to: Span) -> Span {
    Span{hi: to.hi, ..from}
}

enum Tree {
    Word(String),
    Arg(P<Expr>),
    Args(P<Expr>),
    Cmd(P<Expr>),
    If(Condition, Vec<Spanned<Tree>>, Vec<Spanned<Tree>>),
    Match(P<Expr>, Vec<Spanned<(Vec<P<Pat>>, Option<P<Expr>>, Vec<Spanned<Tree>>)>>),
}

fn get_trees_span(trees: &[Spanned<Tree>]) -> Span {
    if trees.is_empty() { DUMMY_SP }
    else { span_from_to(trees[0].span, trees.last().unwrap().span) }
}

fn generate(cx: &mut ExtCtxt, sp: Span, mut trees: Vec<Spanned<Tree>>) -> Result<P<Block>, ()> {
    if trees.is_empty() {
        cx.span_err(sp, "This macro needs at least the command name");
        return Err(());
    }

    let Spanned{span, node: cmd_tree} = trees.remove(0);

    // Not using quote for Command::new($e), because I want to put proper spans in expressions.
    // let new_expr = quote_expr!(cx, ::std::process::Command::new);
    let new_expr = cx.expr_path(cx.path_global(span, vec![
        Ident::with_empty_ctxt(intern("std")),
        Ident::with_empty_ctxt(intern("process")),
        Ident::with_empty_ctxt(intern("Command")),
        Ident::with_empty_ctxt(intern("new")),
    ]));
    let cmd_expr = match cmd_tree {
        Tree::Word(string) => {
            let str_lit = cx.expr_str(span, intern_and_get_ident(&string));
            cx.expr_call(span, new_expr, vec![str_lit])
        }
        Tree::Arg(e) => cx.expr_call(span, new_expr, vec![e]),
        Tree::Cmd(e) => e,
        _ => {
            cx.span_err(span, "Command name should be `cmd` `(cmd_name_expr)` or `{Command_expr}`");
            return Err(());
        }
    };

    let mut stmts: Vec<Stmt> = vec![quote_stmt!(cx, let mut cmd = $cmd_expr).unwrap()];
    stmts.extend(generate_inner(cx, trees)?);

    Ok(cx.block(span, stmts, Some(quote_expr!(cx, cmd))))
}

fn generate_inner(cx: &mut ExtCtxt, trees: Vec<Spanned<Tree>>) -> Result<Vec<Stmt>, ()> {

    // Not using quote for cmd.arg(&$e), because I want to put proper spans in expressions.
    let cmd_expr = quote_expr!(cx, cmd);
    let arg_ident = Ident::with_empty_ctxt(intern("arg"));
    let args_ident = Ident::with_empty_ctxt(intern("args"));

    trees.into_iter().map(|Spanned{span, node: tree}| {
        let x = match tree {
            Tree::Word(string) => {
                let str_lit = cx.expr_str(span, intern_and_get_ident(&string));
                quote_expr!(cx, cmd.arg($str_lit))
            }
            Tree::Arg(e) => {
                let reffed = cx.expr_addr_of(span, e);
                cx.expr_method_call(span, cmd_expr.clone(), arg_ident, vec![reffed])
            }
            Tree::Args(e) => {
                let reffed = cx.expr_addr_of(span, e);
                cx.expr_method_call(span, cmd_expr.clone(), args_ident, vec![reffed])
            }
            Tree::Cmd(_) => {
                cx.span_err(span, "The {} mode doesn't make sense for arguments. Use () instead");
                return Err(());
            }
            Tree::Match(expr, arms) => {
                let arms = arms.into_iter().map(
                    |Spanned{span, node: (pats, guard, trees)}| {
                        let block = generate_block(cx, trees)?;
                        let block = cx.expr_block(block);
                        let mut arm = cx.arm(span, pats, block);
                        arm.guard = guard;
                        Ok(arm)
                    }
                ).collect()?;
                cx.expr_match(span, expr, arms)
            }
            Tree::If(cond, then, els) => {
                let then = generate_block(cx, then)?;
                let els = generate_block(cx, els)?;
                match cond {
                    Condition::Bool(e) => quote_expr!(cx, if $e $then else $els),
                    Condition::IfLet(p, e) => quote_expr!(cx, if let $p = $e $then else $els)
                }
            }
        };
        Ok(cx.stmt_expr(x))
    }).collect()
}

fn generate_block(cx: &mut ExtCtxt, trees: Vec<Spanned<Tree>>) -> Result<P<Block>, ()> {
    let span = get_trees_span(&trees);
    let stmts = generate_inner(cx, trees)?;
    Ok(cx.block(span, stmts, None))
}

struct Parser<'a, 'b: 'a> {
    cx: &'a mut ExtCtxt<'b>,
    p: parser::Parser<'a>,
}

impl<'a, 'b: 'a> Parser<'a, 'b> {
    pub fn new(cx: &'a mut ExtCtxt<'b>, tts: &'a[TokenTree]) -> Parser<'a, 'b> {
        let p = cx.new_parser_from_tts(tts);
        Parser { cx: cx, p: p }
    }

    pub fn parse_trees(&mut self) -> Result<Vec<Spanned<Tree>>, ()> {
        let mut trees = vec![];
        while self.p.token != Token::Eof {
            trees.push(self.parse_tree()?)
        }
        Ok(trees)
    }

    fn parse_tree(&mut self) -> Result<Spanned<Tree>, ()> {
        let tree =
            if self.p.check_keyword(keywords::If) {
                self.parse_if()
            } else if self.p.check_keyword(keywords::Match) {
                self.parse_match()
            } else if self.check_opening() {
                self.parse_splice()
            } else {
                self.parse_word(String::new())
            }?
        ;
        if self.touches_next() {
            self.cx.span_err(tree.span, "This argument should be separated by whitespace");
            return Err(())
        }
        Ok(tree)
    }

    fn check_opening(&self) -> bool {
        if let Token::OpenDelim(_) = self.p.token { true }
        else { false }
    }

    fn touches_next(&self) -> bool {
        if self.p.token == Token::Eof { false }
        else { self.p.last_span.hi == self.p.span.lo }
    }

    fn parse_word(&mut self, already: String) -> Result<Spanned<Tree>, ()> {
        let start_span = self.p.span;
        let word_span;
        let mut word = already;
        loop {
            if self.check_opening() {
                return self.err_next("Parenthesized blocks have to be preceded by whitespace to avoid ambiguity")
            }
            
            let span = self.p.span;
            word.push_str(&self.parse_token_as_string()?);

            if !self.touches_next() {
                word_span = span_from_to(start_span, span);
                break;
            }
        }
        Ok(respan(word_span, Tree::Word(word)))
    }

    fn parse_token_as_string(&mut self) -> Result<String, ()> {
        if let Token::Literal(lit, _) = self.p.token {
            match lit {
                Lit::Char(..) | Lit::Str_(..) | Lit::StrRaw(..) => {
                    let span = self.p.span;
                    let warn = |s: &mut Parser| {
                        if s.touches_next() {
                            s.cx.span_warn(
                                span,
                                "String literals should cover the whole word to avoid confusion"
                            );
                        }
                    };
                    warn(self);
                    match self.p.parse_lit_token() {
                        Ok(LitKind::Char(c)) => {
                            warn(self);
                            return Ok(c.to_string())
                        }
                        Ok(LitKind::Str(s, _)) => {
                            warn(self);
                            return Ok(s.to_string())
                        }
                        _ => unreachable!()
                    }
                }
                Lit::Byte(..) => {
                    return self.err_next("You can't use byte literals in this macro")
                }
                Lit::ByteStr(..) | Lit::ByteStrRaw(..) => {
                    return self.err_next("You can't use bytestring literals in this macro")
                }
                _ => (),
            }
        }
        let stringified = self.cx.parse_sess.codemap().span_to_snippet(self.p.span).unwrap();
        self.p.bump();
        Ok(stringified)
    }
    
    fn parse_naked_keyword(&mut self) -> Result<bool, ()> {
        let span = self.p.span;
        self.p.bump();
        if self.touches_next() {
            if self.check_opening() {
                self.cx.span_err(
                    span,
                    "This keyword should be separated by whitespace or put in a string literal",
                );
                return Err(())
            }
            Ok(false)
        } else {
            Ok(true)
        }
    }

    // Assumes the first token is if keyword
    fn parse_if(&mut self) -> Result<Spanned<Tree>, ()> {
        let if_span = self.p.span;
        if !self.parse_naked_keyword()? {
            return self.parse_word("if".into());
        }
        let condition = 
            if self.p.eat_keyword(keywords::Let) {
                let pat = self.p.parse_pat().map_err(|mut e| e.emit())?;
                self.p.expect(&Token::Eq).map_err(|mut e| e.emit())?;
                let expr = self.p.parse_expr_res(Restrictions::RESTRICTION_NO_STRUCT_LITERAL, None)
                    .map_err(|mut e| e.emit())?;

                Condition::IfLet(pat, expr)
            } else {
                let expr = self.p.parse_expr_res(Restrictions::RESTRICTION_NO_STRUCT_LITERAL, None)
                    .map_err(|mut e| e.emit())?;

                Condition::Bool(expr)
            }
        ;
        let then_block = self.parse_block()?;
        let else_block =
            if self.p.eat_keyword(keywords::Else) {
                if self.p.check_keyword(keywords::If) {
                    vec![self.parse_if()?]
                } else {
                    self.parse_block()?
                }
            } else {
                vec![]
            }
        ;
        Ok(respan(
            span_from_to(if_span, self.p.last_span),
            Tree::If(condition, then_block, else_block)
        ))
    }

    // Assumes the fist token is match keyword
    fn parse_match(&mut self) -> Result<Spanned<Tree>, ()> {
        let match_kw_span = self.p.span;
        if !self.parse_naked_keyword()? {
            return self.parse_word("match".into());
        }
        let expr = self.p.parse_expr_res(Restrictions::RESTRICTION_NO_STRUCT_LITERAL, None)
            .map_err(|mut e| e.emit())?;

        if !self.p.check(&Token::OpenDelim(DelimToken::Brace)) {
            return self.err_next("A {block} was expected after a match expression")
        }

        let tt = self.p.parse_token_tree().map_err(|e| {e}.emit())?;
        let delimited = if let TokenTree::Delimited(_, d) = tt { d } else { unreachable!() };

        let mut p = Parser::new(self.cx, &delimited.tts);

        let mut arms = vec![];
        loop {
            let mut pats = vec![];
            loop {
                pats.push(p.p.parse_pat().map_err(|e| {e}.emit())?);
                if p.p.check(&Token::BinOp(token::Or)) {
                    p.p.bump();
                } else {
                    break;
                }
            }

            let guard = if p.p.eat_keyword(keywords::If) {
                Some(p.p.parse_expr().map_err(|e| {e}.emit())?)
            } else { None };

            p.p.expect(&Token::FatArrow).map_err(|e|{e}.emit())?;

            let block = p.parse_block()?;
            if p.p.check(&Token::Comma) { p.p.bump(); }

            arms.push(respan(
                span_from_to(pats[0].span, p.p.last_span),
                (pats, guard, block),
            ));

            if p.p.check(&Token::Eof) {
                break;
            }
        }
        Ok(respan(span_from_to(match_kw_span, delimited.close_span), Tree::Match(expr, arms)))
    }

    fn parse_block(&mut self) -> Result<Vec<Spanned<Tree>>, ()> {
        if self.p.check(&Token::OpenDelim(DelimToken::Brace)) {
            let tt = self.p.parse_token_tree().unwrap();
            if let TokenTree::Delimited(_, delimited) = tt {
                Parser::new(self.cx, &delimited.tts).parse_trees()
            } else {
                unreachable!()
            }
        } else {
            self.err_next("A {block} was expected")
        }
    }

    // Assumes the first token is Token::OpenDelim(_)
    fn parse_splice(&mut self) -> Result<Spanned<Tree>, ()> {
        let tt = self.p.parse_token_tree().unwrap();
        if let TokenTree::Delimited(span, delimited) = tt {

            // {} is treated as "{}"
            if delimited.tts.is_empty() {
                if delimited.delim == DelimToken::Brace && span.lo.0 + 2 == span.hi.0 {
                    return Ok(respan(span, Tree::Word("{}".into())))
                } else {
                    self.cx.span_err(span, "Rust expression expected inside this block");
                    return Err(())
                }
            }

            let mut p = self.cx.new_parser_from_tts(&delimited.tts);
            let expr = p.parse_expr().map_err(|mut e| e.emit())?;

            p.expect(&Token::Eof).map_err(|e| {e}.emit())?;
            Ok(respan(
                span,
                match delimited.delim {
                    DelimToken::Paren => Tree::Arg(expr),
                    DelimToken::Bracket => Tree::Args(expr),
                    DelimToken::Brace => Tree::Cmd(expr),
                }
            ))
        } else {
            unreachable!()
        }
    }

    fn err_next<T>(&mut self, msg: &str) -> Result<T, ()> {
        self.cx.span_err(self.p.span, msg);
        Err(())
    }
}
