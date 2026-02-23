use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    if let Err(e) = jsina::cli::run(&args) {
        eprintln!("error: {}", e);
        std::process::exit(1);
    }
}
