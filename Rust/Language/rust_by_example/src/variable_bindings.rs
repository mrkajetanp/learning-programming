pub fn variable_bindings() {
    println!("***Variable Bindings***");

    basics();
    mutability();
    scope_and_shadowing();
    declare_first();

    println!("");
}

fn basics() {
    let an_int = 1u32;
    let a_bool = true;
    let unit = ();

    // copy an_int onto copied_int
    let copied_int = an_int;

    println!("an int: {:?}", copied_int);
    println!("a bool: {:?}", a_bool);
    println!("meet the unit: {:?}", unit);

    let _unused_variable = 3u32;

    // let noisy_unused_variable = 2u32;
}

fn mutability() {
    let _immutable_binding = 1;
    let mut mutable_binding = 1;

    println!("before mutation: {}", mutable_binding);

    mutable_binding += 1;

    println!("after mutation: {}", mutable_binding);

    // Error!
    // _immutable_binding += 2;
}

fn scope_and_shadowing() {
    let long_lived_binding = 1;

    {
        let short_lived_binding = 2;

        println!("inner short: {}", short_lived_binding);

        // shadows the outer one
        let long_lived_binding = 5_f32;

        println!("inner long: {}", long_lived_binding);
    }

    println!("outer long: {}", long_lived_binding);

    // also shadows the previous binding
    let long_lived_binding = 'a';

    println!("outer long: {}", long_lived_binding);
}

fn declare_first() {
    // it's possible to forward-declare a variable binding
    let a_binding;

    {
        let x = 2;
        // initalization here
        a_binding = x * x;
    }

    println!("a_binding: {}", a_binding);

    let another_binding;

    // can't use an uninitialized binding
    // println!("another binding: {}", another_binding);

    another_binding = 1;

    println!("another binding: {}", another_binding);
}
