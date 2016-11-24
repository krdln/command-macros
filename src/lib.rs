//! Macros for creating
//! [`std::process::Command`](https://static.rust-lang.org/doc/master/std/process/struct.Command.html)
//! with shell-like syntax.
//!
//! The `command!` macro is a syntax extension and requires nightly,
//! the `cmd!` is simpler version built using `macro_rules!`.
//!
//! This page describes syntax used by both `command!` and `cmd!` macros.
//! See the [github page](https://github.com/krdln/command-macros) for more general introduction.
//!
//! Features marked with \* are unavailable for `cmd!`.
//!
//! ## Naked idents
//!
//! First ident is treated as command name,
//! the rest are parsed as arugments. This invocation:
//!
//! ```
//! # #[macro_use] extern crate command_macros;
//! # fn main() {
//! cmd!(echo foo bar).status().unwrap();
//! # }
//! ```
//!
//! expands to
//!
//! ```ignore
//! {
//!     let cmd = ::std::process::Command::new("echo");
//!     cmd.arg("foo");
//!     cmd.arg("bar");
//!     cmd
//! }.status().unwrap()
//! ```
//!
//! ## `(expression)` (OsStr expression)
//!
//! Interior of `( )` is parsed as Rust expression
//! which should evaluate to `T: AsRef<OsStr>`.
//! This will be put in `cmd.arg(& $expr)`.
//! The `&` is added automatically, like in `println!`,
//! to prevent accidentally moving arguments.
//!
//! ```no_run
//! # #[macro_use] extern crate command_macros;
//! # fn main() {
//! let filename = String::from("foo bar");
//! let get_command = || "touch";
//! cmd!( (get_command()) (filename) ).status().unwrap();
//! # }
//! ```
//!
//! ## `((expression))` (ToString expression)
//!
//! Interior of `(( ))` is parsed as Rust expression
//! which should evaluate to `T: ToString`.
//! Similar rules as with `( )` apply.
//!
//! The following should echo `4`
//!
//! ```
//! # #[macro_use] extern crate command_macros;
//! # fn main() {
//! cmd!( echo ((2+2)) ).status().unwrap();
//! # }
//! ```
//!
//! ## `[expression]` (args expression)
//!
//! Interior of `[ ]` is parsed as Rust expression
//! which should evaluate to `[T: AsRef<OsStr>]`
//! (modulo `Deref`).
//! This expression will be put in `cmd.args(& $expr)`
//!
//! ```no_run
//! # #[macro_use] extern crate command_macros;
//! # fn main() {
//! let args: Vec<_> = std::env::args_os().collect();
//! cmd!( (args[1]) [args[2..]] ).status().unwrap();
//! # }
//! ```
//!
//! ## `{expression}` (Command expression)
//!
//! Interior of `{ }` is parsed as Rust expression
//! which should evaluate to `Command` or `&mut Command`
//! (or anything that has `arg` and `args` methods).
//! It is allowed only at the beginning of macro.
//! It is helpful when you want to append arguments to
//! existing command:
//!
//! ```no_run
//! # #[macro_use] extern crate command_macros;
//! # fn main() {
//! let mut cmd = ::std::process::Command::new("echo");
//! cmd!( {&mut cmd} bar baz ).status().unwrap();
//! # }
//! ```
//!
//! ## Strings\*
//!
//! String literals work like in shell – they expand to single argument or part of it.
//! Character literals and raw string literals are also supported.
//! Note that shell-style `"$variables"` won't work here.
//!
//! ```ignore
//! command!("echo" "single argument" "***")
//! ```
//!
//! `cmd!` workaroud:
//!
//! ```ignore
//! cmd!(echo ("single argument") ("***"))
//! ```
//!
//! ## Arbitrary tokens\*
//!
//! Everything that is not [block], {block}, (block) or string literal,
//! will be stringified. This is mostly helpful for unix-like flags.
//! Everything within a single whitespace-separated chunk will be treated
//! as a single argument. In the following example we are passing
//! three arguments to a `foo.2.5` command.
//!
//! ```ignore
//! command!(foo.2.5 --flag   -v:c -=|.|=-).status().unwrap();
//! ```
//!
//! `cmd!` workaround: `("--flag")`.
//!
//! ## `(-flags)`
//!
//! When your flag contains only `-+=,.;:`
//! and idents, you can omit the quotes. The flag has to start
//! with `-` or `+`.
//!
//! This is only necessary for `cmd!`,
//! when using `command!`, it will generate warning,
//! as you can just remove the surrounding parens.
//!
//! So instead
//!
//! ```ignore
//! cmd!(foo ("--bar=baz"))
//! ```
//!
//! you can write
//!
//! ```ignore
//! cmd!(foo (--bar=baz)
//! ```
//!
//! Please note that all whitespace will be ignored.
//!
//! ## Multi-part arguments\*
//!
//! You can mix `((e))`, `(e)`, tokens and strings within a single
//! arguments as long as they are not separated by whitespace.
//! The following will touch the `foo.v5.special edition` file.
//!
//! ```ignore
//! let p = Path::new("foo");
//! let version = 5;
//! command!(touch (p).v((version))".special edition")
//! ```
//!
//! This is roughly equivalent to `(format!(...))`, but the macro version
//! can handle `OsStr`s (such as `Path`).
//!
//! Please note that this is **not** supported by `cmd!`, which would evaluate
//! every part as *separate* argument.
//!
//! ## `{}`\*
//!
//! Empty `{}` is treated as `"{}"`. This is handy when using commands like `find`.
//! There has to be no space between braces.
//!
//!
//! ## If
//!
//! The `if` token should be surrounded by whitespace.
//! The expression (and pattern in if-let) is parsed as Rust,
//! the inside of {block} is parsed using regular `commmand!` syntax
//! and can evaluate to multiple arguments (or 0).
//! The following should pass `--number` `5` to command `foo`.
//!
//! ```ignore
//! let bar = 5;
//! let option = Some(5);
//! command!(foo
//!     if bar > 10 { zzz }
//!     else if let Some(a) = option { --number ((a)) }
//! ).status().unwrap();
//! ```
//!
//! `cmd!` limitations: `else if` is not supported, expression has to be in parens.
//!
//! ```ignore
//! cmd!(foo
//!     if (bar > 10) { zzz }
//!     else { if let Some(a) = (option) { ("--number") ((a)) } }
//! ).status().unwrap();
//! ```
//!
//! ## Match
//!
//! The `match` token should be surrounded by whitespace.
//! The expression and patterns are parsed as Rust.
//! On the right side of `=>` there should be a {block},
//! which will be parsed (similarly to if blocks)
//! using regular `command!` syntax.
//!
//! This example will pass a single argument `yes` to `foo` command.
//!
//! ```ignore
//! let option = Some(5);
//! command!(foo
//!     match option {
//!         Some(x) if x > 10 => {}
//!         _ => { yes }
//!     }
//! ).status().unwrap()
//! ```
//!
//! `cmd!` limitation: expression after `match` has to be in parens.
//!
//! ## For
//!
//! The `for` token should be surrounded by whitespace.
//! The expression and patterns are parsed as Rust.
//! The interior of block is parsed using `command!` syntax,
//! and will be evaluated in every iteration.
//!
//! This example will pass three arguments `1` `2` `3`.
//!
//! ```ignore
//! command!(echo
//!     for x in 1..4 {
//!         ((x))
//!     }
//! ).status().unwrap()
//! ```

#![cfg_attr(
    feature = "nightly",
    feature(
        plugin_registrar,
        rustc_private,
        quote,
    ),
)]

#![cfg_attr(not(stable_question_mark), feature(question_mark))]

#[cfg(feature = "nightly")] mod plugin;

#[cfg(feature = "nightly")] extern crate syntax;
#[cfg(feature = "nightly")] extern crate rustc;
#[cfg(feature = "nightly")] extern crate rustc_plugin;

#[cfg(feature = "nightly")]
#[plugin_registrar]
pub fn plugin_registrar(reg: &mut rustc_plugin::Registry) {
    reg.register_macro("command", plugin::expand_command);
}

/// Simple macro for creating `Command`.
///
/// Please read the syntax description in the crate's [documentation](index.html).
///
/// Please note that this macro is **not** whitespace-sensitive and the following
/// will evaluate to four separate arguments (as opposed to one in `command!`):
///
/// ```ignore
/// cmd!(echo (foo)(bar)baz(qux)) // don't do it!
/// ```
///
/// # Examples
///
/// ```
/// #[macro_use] extern crate command_macros;
///
/// fn main() {
///     cmd!( echo ((2+2)) ).status().unwrap();
/// }
/// ```
#[macro_export]
macro_rules! cmd {
    ({$e:expr}) => ($e);

    // arg ToString splice
    ({$e:expr} (($a:expr)) $($tail:tt)*) =>
    {
        {
            let mut cmd = $e;
            cmd.arg((&$a).to_string());
            cmd!( {cmd} $($tail)* )
        }
    };

    // (-flag)
    (@stringify {}) => { "" };
    (@stringify {- $($rest:tt)*}) => { concat!("-", cmd!(@stringify {$($rest)*})) };
    (@stringify {+ $($rest:tt)*}) => { concat!("+", cmd!(@stringify {$($rest)*})) };
    (@stringify {= $($rest:tt)*}) => { concat!("=", cmd!(@stringify {$($rest)*})) };
    (@stringify {, $($rest:tt)*}) => { concat!(",", cmd!(@stringify {$($rest)*})) };
    (@stringify {. $($rest:tt)*}) => { concat!(".", cmd!(@stringify {$($rest)*})) };
    (@stringify {; $($rest:tt)*}) => { concat!(";", cmd!(@stringify {$($rest)*})) };
    (@stringify {: $($rest:tt)*}) => { concat!(":", cmd!(@stringify {$($rest)*})) };
    (@stringify {$i:ident $($rest:tt)*}) => {
        // "soon'ah"
        // stringify!($i)
        concat!(stringify!($i), cmd!(@stringify {$($rest)*}))
    };
    ({$e:expr} (-$($tts:tt)*) $($tail:tt)*) => {
        cmd!({$e} (cmd!(@stringify {-$($tts)*})) $($tail)*)
    };
    ({$e:expr} (+$($tts:tt)*) $($tail:tt)*) => {
        cmd!({$e} (cmd!(@stringify {+$($tts)*})) $($tail)*)
    };

    // arg splice
    ({$e:expr} ($a:expr) $($tail:tt)*) => 
    {
        {
            let mut cmd = $e;
            cmd.arg(&$a);
            cmd!( {cmd} $($tail)* )
        }
    };

    // args splice
    ({$e:expr} [$aa:expr] $($tail:tt)*) => {
        {
            let mut cmd = $e;
            cmd.args(&$aa);
            cmd!( {cmd} $($tail)* )
        }
    };

    // match
    ({$e:expr} match ($m:expr) { $($($p:pat)|+ $(if $g:expr)* => {$($rr:tt)*} ),* } $($tail:tt)*) => {
        cmd!({$e} match ($m) { $($($p)|+ $(if $g)* => {$($rr)*})* } $($tail)*)
    };
    ({$e:expr} match ($m:expr) { $($($p:pat)|+ $(if $g:expr)* => {$($rr:tt)*},)* } $($tail:tt)*) => {
        cmd!({$e} match ($m) { $($($p)|+ $(if $g)* => {$($rr)*})* } $($tail)*)
    };
    ({$e:expr} match ($m:expr) { $($($p:pat)|+ $(if $g:expr)* => {$($rr:tt)*} )* } $($tail:tt)*) => {
        {
            let cmd = $e;
            cmd!( {match $m { $($($p)|+ $(if $g)* => cmd!({cmd} $($rr)*)),* }} $($tail)* )
        }
    };

    // if let
    ({$e:expr} if let $p:pat = ($m:expr) { $($then:tt)* } else { $($els:tt)* } $($tail:tt)*) => {
        {
            let cmd = $e;
            cmd!( {
                    if let $p = $m { cmd!({cmd} $($then)*) } else { cmd!({cmd} $($els)*) }
                  } $($tail)*)
        } 
    };
    ({$e:expr} if let $p:pat = ($m:expr) { $($then:tt)* } $($tail:tt)*) => {
        cmd!( {$e}if let $p = ($m) { $($then)* } else {} $($tail)* )
    };

    // if else
    ({$e:expr} if ($b:expr) { $($then:tt)* } else { $($els:tt)* } $($tail:tt)*) => {
        {
            let cmd = $e;
            cmd!( {
                    if $b { cmd!({cmd} $($then)*) } else { cmd!({cmd} $($els)*) }
                  } $($tail)*)
        } 
    };
    ({$e:expr} if ($b:expr) { $($then:tt)* } $($tail:tt)*) => {
        cmd!( {$e}if ($b) { $($then)* } else {} $($tail)* )
    };

    // for
    ({$e:expr} for $p:pat in ($i:expr) { $($body:tt)* } $($tail:tt)*) => {
        {
            let mut cmd = $e;
            for $p in $i { cmd = cmd!( {cmd} $($body)* ); }
            cmd
        }
    };

    // naked ident
    ({$e:expr} $a:ident $($tail:tt)*) => (cmd!( {$e} (stringify!($a)) $($tail)* ));

    // Main entry points (command name)
    (($c:expr) $($tail:tt)*) => {
        cmd!( {::std::process::Command::new(&$c)} $($tail)* )
    };
    ($c:ident $($tail:tt)*) => (cmd!( (stringify!($c)) $($tail)* ));
}

#[cfg(test)]
use ::std::process::Command;

#[test]
fn expr() {
    let mut base: Command = cmd!(echo a);
    base.env("FOO", "bar");
    quicktest(cmd!({base} b), "a b");
}

#[cfg(test)]
fn quicktest(mut echocmd: Command, target: &str) {
    let out = echocmd.output().expect("quicktest: can't echo").stdout;
    assert_eq!(String::from_utf8_lossy(&out).trim(), target);
}

#[test]
fn simple() {
    let output = cmd!(echo raz dwa trzy).output().expect("can't echo");
    assert_eq!(output.stdout, &b"raz dwa trzy\n"[..]);
}

#[test]
fn ffmpeg() {
    let moreargs = ["-pix_fmt", "yuv420p"];
    let file = "file.mp4".to_string();
    let preset = "slow";
    let tmpname = "tmp.mkv";
    let output = cmd!(echo
            (-i) (file)
            (-c:v) libx264 (-preset) (preset) [moreargs]
            (-c:a) copy
            (tmpname))
        .output()
        .expect("can't echo");
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        "-i file.mp4 -c:v libx264 -preset slow -pix_fmt yuv420p -c:a copy tmp.mkv\n"
    );
}

#[test]
fn match_test() {
    let option = Some(5);

    quicktest(
        cmd!(echo
            match (option) {
                Some(x) => {("--number") (x.to_string())}
                None => {}
            }
            tail),
        "--number 5 tail"
    );

    for &(x, target) in &[
        (Ok(1), ". 0101 ."),
        (Ok(5), ". small 5 ."),
        (Ok(10), ". 10 ."),
        (Err("bu"), ". err bu ."),
    ] {
        quicktest(cmd!(
                echo (".") match (x) {
                    Ok(0) | Ok(1) => { ("0101") },
                    Ok(x) if x < 7 => { small (x.to_string()) },
                    Ok(x) => { (x.to_string()) },
                    Err(x) => { err (x) }
                } (".")
            ),
            target
        );
    }
}

#[test]
fn iflet() {
    let option = Some(5);
    quicktest(
        cmd!(echo
            if let Some(x) = (option) { ("--number") ((x)) }
            tail),
        "--number 5 tail"
    );

    let option: Option<()> = None;
    quicktest(
        cmd!(echo
            if let Some(_) = (option) {} else { ok }
            tail),
        "ok tail"
    );
}


#[test]
fn ifelse() {
    quicktest(
        cmd!(echo
            if (true) { abc (1.to_string()) }
            tail),
        "abc 1 tail"
    );

    let counter = ::std::cell::Cell::new(0);
    quicktest(
        cmd!(echo
            ({counter.set(counter.get() + 1); "blah"})
            if (true) { a } else { b }
            if (false) { c } else { d }
            tail),
        "blah a d tail"
    );
    assert_eq!(counter.get(), 1);
}

#[test]
fn test_mutref() {
    let cmd = &mut Command::new("echo");
    let cmd: &mut Command = cmd!({cmd} foo);
    assert_eq!(cmd.output().unwrap().stdout, &b"foo\n"[..]);
}

#[test]
fn test_parenparen() {
    quicktest(cmd!( echo ((2+2)) ), "4");
    let foo = || "a";
    quicktest(cmd!( echo ((foo)()) ), "a");
}

#[test]
fn for_loop() {
    quicktest(cmd!(
            echo
            for x in (&["a", "b"]) {
                foo (x)
            }
            end
        ),
        "foo a foo b"
    );
}

#[test]
fn not_moving() {
    let s = String::new();
    cmd!((s));
    cmd!(((s)));
    cmd!((s));
}

#[test]
fn flags() {
    quicktest(
        cmd!(echo (".") (-e) (-c:a=aac) (+a:b;c,d.txt)),
        ". -e -c:a=aac +a:b;c,d.txt",
    );
}

