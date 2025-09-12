use std::env;

use parse::session::ParseSess;

fn main() {
    let files = if env::args().len() > 1 {
        env::args().skip(1).collect::<Vec<String>>()
    } else {
        vec!["testdata/small.HC".to_string()]
    };
    let mut sesh = ParseSess::new(files);
    dbg!(&sesh);
    sesh.mk_asteez();
}
