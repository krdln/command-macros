# command-macros

[**documentation**](http://krdln.github.io/command-macros/command_macros/index.html),
[**crate**](https://crates.io/crates/command-macros)

Macros for creating [`std::process::Command`](https://static.rust-lang.org/doc/master/std/process/struct.Command.html)
with shell-like syntax.
Created to make using Rust as a scripting language more pleasant.

This crate contains two macros, `command!()` – fully-featured,
but requires nightly, and a simpler [`cmd!()`](#cmd), built by `macro_rules`.

## `command!`

### Installation

This macro requires nighlty Rust and
enabling a "nightly" feature.
Put the following in your `Cargo.toml`.

```toml
[dependencies.command-macros]
version = "0.1.1"
features = ["nightly"]
```

When using nightly from before 2016-06-10 use `version = 0.1.0`.

And then add on top of your root module:
```rust
#![feature(plugin)]
#![plugin(command_macros)]
```

### Examples

```rust
command!(
    ffmpeg -i (file)
    -c:v libx264 -preset (preset) [moreargs]
    -c:a copy
    file:(tmpname)
).status().unwrap();
```

should be roughly equivalent to running

```rust
std::process::Command::new("ffmpeg")
    .args(&["-i", &file])
    .args(&["-c:v", "libx264", "-preset", &preset])
    .args(&moreargs)
    .args(&["-c:a", "copy"])
    .arg(&format!("file:{}", tmpname))
    .status()
    .unwrap();
```

As you see, you use `(expr)` to create an argument (or a part of it)
from arbitrary Rust expression and `[expr]` for multiple arguments.
The `&` is added automatically, similarly to how `print!` works.

Moreover, `command!` will handle `file` and `tmpname` being `OsStr` correctly,
while the manual version would require some modifications.

Additionally, you can use `if`, `if let`, `match` and `for`.
This snippet also showcases `(( expr ))` feature.

```rust
command!(make
    if let Some(n) = n_cores { -j ((n + 1)) }
).status().unwrap();
```

Both macros return `Command` by value, so you can store them in a variable for later:

```rust
let cmd = command!(mkv --fs);
```

If you have partially prepared command (`Command` or `&mut Command`),
you can also pass it to this macro:

```rust
let base: Command = prepare();
let cmd = command!({cmd} install (package));
```

## `cmd!`

### Installation

Put the following in your `Cargo.toml`.

```toml
[dependencies]
command-macros = "0.1"
```

And then add on top of your root module:
```rust
#[macro_use] extern crate command_macros;
```

### Limitations

This macro is a "lite" version of the `command!`.
Differences:
* Worse error messages.
* It is whitespace-insensitive.
* Creating arguments from arbitrary tokens (`-c:a`) is not supported (only idents).
* `((expr))` and `(expr)` always evaluate to full argument (no tricks like `file:(filename)`).
* Expressions in `if`, `match` and `for` have to be surrounded by parens.

Besides, all other features should work.

### Examples

Examples from `command!` section rewritten to match `cmd!` syntax:

```rust
command!(
    ffmpeg ("-i") (file)
    ("-c:v") libx264 ("-preset") (preset) [moreargs]
    ("-c:a") copy
    (format!("file:{}", tmpname))
).status().unwrap();
```

```rust
command!(make
    if let Some(n) = (n_cores) { ("-j") ((n + 1)) }
).status().unwrap();
