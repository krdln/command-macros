# Changelog

# 0.2.0 — 2017-05-02

* Reimplemented `command` using new `proc_macro`
* Moved `command` macro impl to separate crate
  (it's a limitation of `proc_macro`)
* Removed the flag shorthand for `cmd` macro `(--foo)`
* Removed auto-reffing expression in `[args]`
  (because args has started(?) to use IntoIterator,
  and `&` would limit flexibility))
