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
                -c:v libx264 -preset (preset) [moreargs]
                -c:a copy
                (tmpname)
            ),
            "-i file.mp4 -c:v libx264 -preset slow -pix_fmt yuv420p -c:a copy tmp.mkv"
        );
    }

    #[test]
    fn ugly() {
        quicktest(command!(echo if {{}; false}{a}else{b}), "b");
        quicktest(command!(echo if-a=5 {} else {}), "if-a=5 {} else {}");
    }
}
