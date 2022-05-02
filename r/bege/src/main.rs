use bege::execution;
use bege::space::{load_text, GenericFungeSpace};
use bege::standards::befunge93;

fn main() {
    let mut space = GenericFungeSpace::<befunge93::Position, u8>::new();

    load_text(&mut "'x,@".as_bytes(), &mut space).unwrap();

    execution::run(
        &[execution::initial_ip()],
        &mut std::io::stdin().lock(),
        &mut std::io::stdout(),
        &mut space,
    );
}
