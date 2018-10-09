# Changelog

# 0.2.6 — 2018-10-09

Update for the latest nightly:

* `proc_macro_non_items` feature gate changed to `proc_macro_hygiene`

# 0.2.5 — 2018-10-05

Update for the latest nightly:

* `Span::def_site` now requires separate `proc_macro_def_site` feature gate
* `use_extern_macros` is no longer needed (since 1.30)

# 0.2.4 — 2018-07-22

Update for the latest nightly due to stabilization of
`proc_macro` feature:

* `proc_macro` feature is no longer needed
* `use_extern_macros` is now needed instead
* Not the whole `proc_macro` API was stabilized, so implementation
  uses `proc_macro_span` and `proc_macro_diagnostics` features.

# 0.2.3 — 2018-05-21

* Update for the latest nightly
* Remove explicit link to documentation

# 0.2.0 — 2018-05-02

* Reimplemented `command` using new `proc_macro`
* Moved `command` macro impl to separate crate
  (it's a limitation of `proc_macro`)
* Removed the flag shorthand for `cmd` macro `(--foo)`
* Removed auto-reffing expression in `[args]`
  (because args has started(?) to use IntoIterator,
  and `&` would limit flexibility))
