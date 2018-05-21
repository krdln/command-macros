//! Helper module with some ad-hoc newtypes over TokenStream and
//! TokenTree with some helper functions.

use proc_macro::{
    TokenStream,
    TokenTree,
    Span,
    Group,
    Delimiter,
    Literal,
    Punct,
    Ident,
    Spacing,
};

use std::iter::once;

#[derive(Debug)]
pub struct Pat(pub TokenStream);

#[derive(Debug)]
pub struct Expr(TokenTree);

#[derive(Debug)]
pub struct Stmt(TokenTree);

impl Expr {
    pub fn into_stmt(self) -> Stmt {
        let stream = once(self.into_tt())
            .chain(once(new_punct(';')))
            .collect();
        Stmt::from_stream(stream)
    }

    pub fn into_tt(self) -> TokenTree {
        let Expr(tt) = self;
        tt
    }

    pub fn into_stream(self) -> TokenStream {
        match self {
            Expr(TokenTree::Group(group)) => {
                if group.delimiter() == Delimiter::None {
                    group.stream()
                } else {
                    once(TokenTree::from(group)).collect()
                }
            }
            expr => once(expr.into_tt()).collect()
        }
    }

    pub fn grouped(self) -> Expr {
        if let Expr(TokenTree::Group(ref old_group)) = self {
            if old_group.delimiter() == Delimiter::None {
                let mut new_group = Group::new(Delimiter::Parenthesis, old_group.stream());
                new_group.set_span(old_group.span());
                return Expr(new_group.into())
            }
        }
        self
    }

    pub fn span(&self) -> Span {
        self.0.span()
    }

    pub fn from_tt(tt: TokenTree) -> Self {
        Expr(tt)
    }
}

impl Stmt {
    fn into_stream(self) -> TokenStream {
        // Just reusing Expr's implementation
        let Stmt(tt) = self;
        Expr(tt).into_stream()
    }
}

// Constructors

impl Expr {
    pub fn from_stream(stream: TokenStream) -> Expr {
        let span = {
            if stream.is_empty() {
                Span::def_site()
            } else {
                let mut stream = stream.clone().into_iter();
                let span = stream.next().unwrap().span();
                if let Some(last) = stream.last() {
                    span.join(last.span()).unwrap()
                } else {
                    span
                }
            }
        };
        Expr(surround(stream, Delimiter::None, span))
    }

    pub fn from_source(source: &'static str, span: Span) -> Expr {
        Expr::from_stream(from_source(source, span).collect())
    }

    pub fn call<Iter>(function: Iter, expr: Expr, span: Span) -> Expr
    where Iter: Iterator<Item=TokenTree>
    {
        let stream = expr.into_stream();
        let stream = function
            .chain(once(surround(stream, Delimiter::Parenthesis, span)))
            .collect();
        Expr::from_stream(stream)
    }

    pub fn call_method(caller: Expr, method: &str, arg: Expr, span: Span) -> Expr {
        let function = caller.grouped().into_stream().into_iter()
            .chain(once(new_spanned_punct('.', span)))
            .chain(once(new_spanned_ident(method, span)));
        Expr::call(function, arg, span)
    }

    pub fn call_method_on(caller: &TokenTree, method: &str, arg: Expr, span: Span) -> Expr {
        Expr::call_method(Expr::from_tt(caller.clone()), method, arg, span)
    }

    pub fn block(stmts: Vec<Stmt>, expr: Expr, span: Span) -> Expr {
        let block = surround(
            stmts.into_iter()
                .map(|Stmt(tt)| tt)
                .chain(once(expr.into_tt()))
                .collect(),
            Delimiter::Brace,
            span,
        );
        Expr(block)
    }

    pub fn string_literal(word: &str) -> Expr {
        Expr(Literal::string(word).into())
    }

    pub fn reference(inner: Expr, span: Span) -> Expr {
        let stream = once(new_spanned_punct('&', span))
            .chain(inner.grouped().into_stream())
            .collect();
        Expr::from_stream(stream)
    }
}

impl Stmt {
    pub fn from_stream(stream: TokenStream) -> Stmt {
        Stmt(surround(stream, Delimiter::None, Span::def_site()))
    }

    pub fn new_let(var: &TokenTree, expr: Expr) -> Stmt {
        let stream = from_source("#[allow(unused)] let mut", Span::call_site())
            .chain(once(var.clone()))
            .chain(once(new_punct('=')))
            .chain(expr.into_stream())
            .chain(once(new_punct(';')))
            .collect();
        Stmt::from_stream(stream)
    }
}

pub fn surround(stream: TokenStream, delimiter: Delimiter, span: Span) -> TokenTree {
    let mut g = Group::new(delimiter, stream);
    g.set_span(span);
    g.into()
}

pub fn from_source(source: &'static str, span: Span) -> impl Iterator<Item=TokenTree> {
    source
        .parse::<TokenStream>()
        .unwrap()
        .into_iter()
        .map(move |mut tt| { tt.set_span(span); tt })
}

pub fn new_punct(punct: char) -> TokenTree {
    Punct::new(punct, Spacing::Alone).into()
}

pub fn new_spanned_punct(punct: char, span: Span) -> TokenTree {
    let mut punct = Punct::new(punct, Spacing::Alone);
    punct.set_span(span);
    punct.into()
}

pub fn new_ident(word: &str) -> TokenTree {
    Ident::new(word, Span::def_site()).into()
}

pub fn new_spanned_ident(word: &str, span: Span) -> TokenTree {
    let mut ident = Ident::new(word, Span::def_site());
    ident.set_span(span);
    ident.into()
}

pub fn new_block(stmts: Vec<Stmt>, span: Span) -> TokenTree {
    let stream = stmts.into_iter().flat_map(|stmt| stmt.into_stream()).collect();
    surround(stream, Delimiter::Brace, span)
}
