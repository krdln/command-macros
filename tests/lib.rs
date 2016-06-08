#![feature(plugin)]

#![plugin(command_macros)]

#[cfg(feature = "nightly")]
mod plugin {
    use std::process::Command;

    fn quicktest(mut echocmd: Command, target: &str) {
        let out = echocmd.output().expect("quicktest: can't echo").stdout;
        assert_eq!(String::from_utf8_lossy(&out).trim(), target);
    }

    #[test]
    fn good() {
        let bar = false;
        let option = Some(5);
        quicktest(
            command!(echo {} if bar {-=bar=-} else if let Some (a) = option {--number (a.to_string())} levano),
            "{} --number 5 levano"
            // "{} -=bar=-"
        );
    }
}
