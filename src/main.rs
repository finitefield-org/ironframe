use ironframe::run_from_env;

fn main() {
    if let Err(err) = run_from_env() {
        eprintln!("error: {}", err.message);
        std::process::exit(1);
    }
}
