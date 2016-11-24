extern crate rustc_version;

use rustc_version::*;

fn main() {
    let meta = version_meta();
    match meta.channel {
        Channel::Nightly | Channel::Beta => {
            if let Some(date) = meta.commit_date {
                if &date[..] > "2016-07" {
                    println!("cargo:rustc-cfg=nightly_from_2016_07");
                }
                if &date[..] >= "2016-10-18" {
                    println!("cargo:rustc-cfg=nightly_from_2016_10_19");
                }
                if &date[..] >= "2016-11-20" {
                    println!("cargo:rustc-cfg=nightly_from_2016_11_21");
                }
            }
        }
        _ => ()
    }
    if version_matches("1.14") {
        println!("cargo:rustc-cfg=stable_question_mark");
    }
}
