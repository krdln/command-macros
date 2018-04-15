#![feature(proc_macro, proc_macro_non_items)]

extern crate command_macros_plugin;

mod plugin {
    use std::process::Command;
    use command_macros_plugin::command;

    fn quicktest(mut echocmd: Command, target: &str) {
        let out = echocmd.output().expect("quicktest: can't echo").stdout;
        assert_eq!(String::from_utf8_lossy(&out).trim(), target);
    }

    #[test]
    fn good() {
        let bar = false;
        let option = Some(5);
        quicktest(
            command!(echo {} if bar {-=bar=-} else if let Some(a) = option {--number (a.to_string())} levano),
            "{} --number 5 levano"
        );
        let bar = true;
        quicktest(
            command!(echo {} if bar {-=bar=-} else if let Some(a) = option {--number (a.to_string())} levano),
            "{} -=bar=- levano"
        );
    }

    #[test]
    fn ffmpeg() {
        let moreargs = ["-pix_fmt", "yuv420p"];
        let file = "file.mp4".to_string();
        let preset = "slow";
        let tmpname = "tmp.mkv";
        quicktest(
            command!(
                echo -i (file)
                -c:v libx264 -preset (preset) [&moreargs]
                -c:a copy
                file:(tmpname)
            ),
            "-i file.mp4 -c:v libx264 -preset slow -pix_fmt yuv420p -c:a copy file:tmp.mkv"
        );
    }

    #[test]
    fn strings() {
        quicktest(command!("echo" r"a~\b"), "a~\\b");
    }

    #[test]
    fn ugly() {
        quicktest(command!(echo if {{}; false}{a}else{b}), "b");
        // quicktest(command!(echo if-a=5 {} else {}), "if-a=5 {} else {}");
        // quicktest(command!(echo else-a {} let-a {}), "else-a {} let-a {}");
    }

    #[test]
    fn match_test() {
        for &(x, target) in &[
            (Ok(1), ". 0101 ."),
            (Ok(5), ". small 5 ."),
            (Ok(10), ". 10 ."),
            (Err("bu"), ". err bu ."),
        ] {
            quicktest(command!(
                    echo . match x {
                        Ok(0) | Ok(1) => { 0101 },
                        Ok(x) if x < 7 => { small (x.to_string()) },
                        Ok(x) => { (x.to_string()) }
                        Err(x) => { err (x) }
                    } .
                ),
                target
            );
        }
    }

    #[test]
    fn parenparen() {
        quicktest(command!(echo ((2+2))), "4");
        fn inc(x: i32) -> String { (x + 1).to_string() };
        quicktest(command!(echo ((inc)(3))), "4");
    }

    #[test]
    fn touching() {
        quicktest(command![echo number((2+2))], "number4");
        quicktest(command![("e")"ch"(('o')) number((2+2))], "number4");
        quicktest(command![echo ("abc")-((5))-def.txt hij], "abc-5-def.txt hij");
    }

    #[test]
    fn for_loop() {
        quicktest(command![
                echo
                for (i, x) in ["a", "b"].iter().enumerate() {
                    --add ((i+1)).(x).txt
                }
                end
            ],
            "--add 1.a.txt --add 2.b.txt end"
        );
        // quicktest(command!(echo for-me), "for-me");
    }

    #[test]
    fn not_moving() {
        let s = String::new();
        command!((s));
        command!(((s)));
        command!((s));
    }

    #[test]
    fn hygiene() {
        let cmd = 42;
        quicktest(command![echo ((cmd))], "42");
    }

    #[test]
    fn flags_warn() {
        // quicktest(command![echo . (--flag) (+c)], ". --flag +c");
    }
}
