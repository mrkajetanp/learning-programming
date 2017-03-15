mod variable_bindings;
mod functions;
mod primitive_types;
mod ifs;
mod loops;
mod vectors;
mod ownership;
mod references_and_borrowing;
mod lifetimes;
mod mutability;
mod structs;
// mod enums;
mod matches;
mod patterns;
mod methods;
mod strings;
mod generics;
mod traits;
mod drop;
mod if_let;
mod trait_objects;
mod closures;
mod universal_fn_syntax_call;
mod const_and_static;
mod attributes;
mod type_aliases;
mod casting;

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
    lifetimes::lifetimes();
    mutability::mutability();
    structs::structs();
    // enums::enums();
    matches::matches();
    patterns::patterns();
    methods::methods();
    strings::strings();
    generics::generics();
    traits::traits();
    drop::drop();
    if_let::if_let();
    trait_objects::trait_objects();
    closures::closures();
    universal_fn_syntax_call::universal_fn_syntax_call();
    const_and_static::const_and_static();
    attributes::attributes();
    type_aliases::type_aliases();
    casting::casting();
}
