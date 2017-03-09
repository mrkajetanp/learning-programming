mod variable_bindings;
mod functions;
mod primitive_types;
mod ifs;
mod loops;
mod vectors;
mod ownership;
mod references_and_borrowing;

///! how does it work?

/// some doc comment
/// # another doc comment

fn main() {
    variable_bindings::variable_bindings();
    functions::functions();
    primitive_types::primitive_types();
    ifs::ifs();
    loops::loops();
    vectors::vectors();
    ownership::ownership();
    references_and_borrowing::references_and_borrowing();
}
