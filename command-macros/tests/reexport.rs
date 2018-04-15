#![cfg_attr(feature = "nightly", feature(proc_macro, proc_macro_non_items))]

#[cfg(feature = "nightly")] extern crate command_macros;

#[cfg(feature = "nightly")]
mod tests {
    #[test]
    fn reexport_via_use() {
        use command_macros::command;
        let _ = command!(foo --bar);
    }
}
