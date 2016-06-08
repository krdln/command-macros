use syntax::ptr::P;
use syntax::codemap::Span;
use syntax::parse::token::{self, Lit, Token, DelimToken};
use syntax::parse::token::keywords;
use syntax::parse::token::intern_and_get_ident;
use syntax::parse::parser::Restrictions;
use syntax::ast::{TokenTree, LitKind, Expr, Stmt, Block, Pat};
use syntax::ext::base::{ExtCtxt, MacResult, DummyResult, MacEager};
use syntax::ext::build::AstBuilder;  // trait for expr_usize

pub fn expand_command(cx: &mut ExtCtxt, sp: Span, args: &[TokenTree])
    -> Box<MacResult + 'static>
{
    let trees = match parse(cx, args) {
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

impl Condition {
    fn get_span(&self) -> Span {
        match *self {
            Condition::Bool(ref e) => e.span,
            Condition::IfLet(ref p, _) => p.span,
        }
    }
}

enum Tree<'a> {
    String(Span, String),
    Arg(P<Expr>),
    Args(P<Expr>),
    Cmd(P<Expr>),
    If(Condition, Vec<Tree<'a>>, Vec<Tree<'a>>),
    Match(Vec<(&'a [TokenTree], Vec<Tree<'a>>)>),
}

fn generate(cx: &mut ExtCtxt, span: Span, mut trees: Vec<Tree>) -> Result<P<Block>, ()> {
    if trees.is_empty() {
        cx.span_err(span, "This macro needs at least the command name");
        return Err(());
    }

    let cmd_expr = match trees.remove(0) {
        Tree::String(span, string) => {
            let str_lit = cx.expr_str(span, intern_and_get_ident(&string));
            quote_expr!(cx, ::std::process::Command::new($str_lit))
        }
        Tree::Arg(e) => quote_expr!(cx, ::std::process::Command::new($e)),
        Tree::Cmd(e) => e.clone(),
        _ => {
            cx.span_err(span, "Command name should be `cmd` `(cmd_name_expr)` or `{Command_expr}`");
            return Err(());
        }
    };

    let mut stmts: Vec<Stmt> = vec![quote_stmt!(cx, let mut cmd = $cmd_expr).unwrap()];
    stmts.extend(generate_inner(cx, trees)?);

    Ok(cx.block(span, stmts, Some(quote_expr!(cx, cmd))))
}

fn generate_inner(cx: &mut ExtCtxt, trees: Vec<Tree>) -> Result<Vec<Stmt>, ()> {
    trees.into_iter().map(|tree| {
        let x = match tree {
            Tree::String(span, string) => {
                let str_lit = cx.expr_str(span, intern_and_get_ident(&string));
                quote_expr!(cx, cmd.arg($str_lit))
            }
            Tree::Arg(e) => quote_expr!(cx, cmd.arg(&$e)),
            Tree::Args(e) => quote_expr!(cx, cmd.args(&$e)),
            Tree::Cmd(e) => {
                cx.span_err(e.span, "The {} mode doesn't make sense for arguments. Use () instead");
                return Err(());
            }
            Tree::Match(..) => unimplemented!(),
            Tree::If(cond, then, els) => {
                // FIXME wrong spans
                let stmts = generate_inner(cx, then)?;
                let then = cx.block(cond.get_span(), stmts, None);
                let stmts = generate_inner(cx, els)?;
                let els = cx.block(cond.get_span(), stmts, None);
                match cond {
                    Condition::Bool(e) => quote_expr!(cx, if $e $then else $els),
                    Condition::IfLet(p, e) => quote_expr!(cx, if let $p = $e $then else $els)
                }
            }
        };
        Ok(cx.stmt_expr(x))
    }).collect()
}

fn stringify_token(cx: &mut ExtCtxt, tt: &TokenTree) -> Result<Option<String>, &'static str> {
    let tts = unsafe { ::std::slice::from_raw_parts(tt, 1) };
    if let TokenTree::Token(_, ref tok) = *tt {
        if let Token::Literal(lit, _) = *tok {
            match lit {
                Lit::Char(..) | Lit::Str_(..) | Lit::StrRaw(..) => {
                    let mut p = cx.new_parser_from_tts(tts);
                    match p.parse_lit_token() {
                        Ok(LitKind::Char(c)) => return Ok(Some(c.to_string())),
                        Ok(LitKind::Str(s, _)) => return Ok(Some(s.to_string())),
                        _ => unreachable!()
                    }
                }
                Lit::Byte(..) => return Err("You can't use byte literals in this macro"),
                Lit::ByteStr(..) | Lit::ByteStrRaw(..) => return Err("You can't use bytestring literals in this macro"),
                _ => (),
            }
        }
    }
    Ok(None)
}

fn parse<'a>(cx: &mut ExtCtxt, args: &'a [TokenTree]) -> Result<Vec<Tree<'a>>, ()> {

    if args.is_empty() { return Ok(vec![]) }


    let mut groups = vec![(vec![&args[0]], 0)];
    for (i, arg) in args.iter().enumerate().skip(1) {
        let is_touching = {
            let last = groups.last().unwrap().0.last().unwrap();
            arg.get_span().lo.0 == last.get_span().hi.0 && {
                for &tt in &[arg, last] {
                    if let TokenTree::Delimited(span, _) = *tt {
                        cx.span_err(
                            span,
                            "Parenthesized blocks have to be separated by whitespace to avoid ambiguity",
                        );
                        return Err(());
                    }
                }
                true
            }
        };
        if is_touching { groups.last_mut().unwrap().0.push(arg); }
        else { groups.push((vec![arg], i)); }
    }

    let mut condition_start = None;
    let mut trees = vec![];

    let mut groups_iter = groups.iter();
    while let Some(&(ref group, i)) = groups_iter.next() {
        if let Some(start) = condition_start {
            match *group[0] {
                TokenTree::Delimited(_, ref block) if block.delim == DelimToken::Brace => {
                    condition_start = None;
                    let tts = &args[start..i];
                    let mut p = cx.new_parser_from_tts(tts);
                    let condition = if p.eat_keyword(keywords::Let) {
                        let pat = match p.parse_pat() {
                            Ok(pat) => pat,
                            Err(mut e) => { e.emit(); return Err(()) }
                        };
                        if p.token == Token::Eq {
                            p.bump();
                        } else {
                            cx.span_err(p.span, "`=` expected after if-let's pattern");
                            return Err(());
                        }
                        match p.parse_expr() {
                            Ok(e) => Condition::IfLet(pat, e),
                            Err(mut e) => { e.emit(); return Err(()) }
                        }
                    } else {
                        let expr = match p.parse_expr() {
                            Ok(e) => e,
                            Err(mut e) => { e.emit(); return Err(()) }
                        };
                        // if p.token == Token::OpenDelim(DelimToken::Brace) {
                        if p.token == Token::Eof {
                            Condition::Bool(expr)
                        } else {
                            cx.span_err(expr.span, "the expression ends too soon");
                            return Err(());
                        }
                    };
                    let then = parse(cx, &block.tts)?;
                    match groups_iter.as_slice().first().map(|&(ref tts, _)| &tts[..]) {
                        Some([&TokenTree::Token(else_span, ref tok)]) if tok.is_keyword(keywords::Else) => {
                            groups_iter.next();
                            match groups_iter.next().map(|&(ref tts, i)| (&tts[..], i)) {
                                Some(([&TokenTree::Delimited(span, ref block)], _)) if block.delim == DelimToken::Brace => {
                                    trees.push(Tree::If(condition, then, parse(cx, &block.tts)?));
                                }
                                Some(([&TokenTree::Token(span, ref tok)], i)) if tok.is_keyword(keywords::If) => {
                                    let mut nested_trees = parse(cx, &args[i..])?;
                                    trees.push(Tree::If(condition, then, vec![nested_trees.remove(0)]));
                                    trees.extend(nested_trees);
                                    break;
                                }
                                _ => {
                                    cx.span_err(else_span, "Expected {block} or `if ` after this else keyword");
                                    return Err(());
                                }
                            }
                        }
                        _ => trees.push(Tree::If(condition, then, vec![]))
                    }
                }
                _ => ()
            } // end match *group[0]
            continue
        } // end if let Some(start) = condition_start

        match &group[..] {
            [&TokenTree::Delimited(span, ref delimited)] => {
                if delimited.tts.is_empty() {
                    if delimited.delim == DelimToken::Brace && span.lo.0 + 2 == span.hi.0 {
                        trees.push(Tree::String(span, "{}".into()));
                        continue;
                    } else {
                        cx.span_err(span, "Rust expression expected inside this block");
                        return Err(())
                    }
                }
                let mut p = cx.new_parser_from_tts(&delimited.tts);
                let expr = match p.parse_expr() {
                    Ok(e) => e,
                    Err(mut e) => { e.emit(); return Err(()) }
                };
                if p.token == Token::Eof {
                    trees.push(
                        match delimited.delim {
                            DelimToken::Paren => Tree::Arg(expr),
                            DelimToken::Bracket => Tree::Args(expr),
                            DelimToken::Brace => Tree::Cmd(expr),
                        }
                    );
                } else {
                    cx.span_err(expr.span, "the expression ends too soon");
                    return Err(());
                }
            }
            [&TokenTree::Token(span, ref tok)] if tok.is_keyword(keywords::If) => {
                condition_start = Some(i+1);
            }
            [&TokenTree::Token(span, ref tok)] if tok.is_keyword(keywords::Match) => {
                cx.span_err(span, "match is unimplemented");
                return Err(());
            }
            _ => {
                let mut word = String::new();
                for &tt in group {
                    match stringify_token(cx, tt) {
                        Ok(Some(s)) => {
                            if group.len() > 1 {
                                cx.span_warn(
                                    tt.get_span(),
                                    "String literals should cover the whole argument to avoid confusion"
                                );
                            }
                            word.push_str(&s);
                        }
                        Ok(None) => word.push_str(
                            &cx.parse_sess.codemap().span_to_snippet(tt.get_span()).unwrap(),
                        ),
                        Err(e) => {
                            cx.span_err(tt.get_span(), e);
                            return Err(());
                        }
                    }
                }
                let mut span = group[0].get_span();
                span.hi = group.last().unwrap().get_span().hi;
                trees.push(Tree::String(span, word));
            }
        } // end match
    } // end for

    if let Some(i) = condition_start {
        cx.span_err(args[i-1].get_span(), "No {block} was found for this if");
        return Err(());
    }

    Ok(trees)
}

