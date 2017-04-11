pub fn _any() {
    println!("*** Any ***", );

    basic();
    typeid_struct();
    any_trait();

    println!("", );
}

fn basic() {
    use std::fmt::Debug;
    use std::any::Any;

    // logger function for any type that implements Debug
    fn log<T: Any + Debug>(value: &T) {
        let value_any = value as &Any;

        // try to convert our value to String
        // if successful, output the String's length and its value
        // if not, it's a different type so just print it out unadorned
        match value_any.downcast_ref::<String>() {
            Some(as_string) =>
                println!("String ({}): {}", as_string.len(), as_string),
            None =>
                println!("{:?}", value),
        }
    }

    fn do_work<T: Any + Debug>(value: &T) {
        log(value);
        // something else ...
    }

    let my_string = "Hello World".to_string();
    do_work(&my_string);

    let my_i8: i8 = 100;
    do_work(&my_i8);
}

fn typeid_struct() {
    use std::any::{Any, TypeId};

    fn is_string<T: ?Sized + Any>(_s: &T) -> bool {
        TypeId::of::<String>() == TypeId::of::<T>()
    }

    assert_eq!(false, is_string(&0));
    assert_eq!(true, is_string(&"test".to_string()));
}

fn any_trait() {

    use std::any::{Any, TypeId};

    {
        fn is_string(s: &Any) -> bool {
            TypeId::of::<String>() == s.get_type_id()
        }

        assert_eq!(false, is_string(&0));
        assert_eq!(true, is_string(&"test".to_string()));
    }

    {
        fn is_string(s: &Any) -> bool {
            s.is::<String>()
        }
    }

    fn print_if_string(s: &Any) {
        if let Some(string) = s.downcast_ref::<String>() {
            println!("It's a string ({}): {}", string.len(), string);
        } else {
            println!("Not a string..");
        }
    }

    print_if_string(&0);
    print_if_string(&"something".to_string());

    fn modify_if_u32(s: &mut Any) {
        if let Some(num) = s.downcast_mut::<u32>() {
            *num = 42;
        }
    }

    let mut x = 10_u32;
    let mut s = "test".to_string();

    modify_if_u32(&mut x);
    modify_if_u32(&mut s);

    assert_eq!(42, x);
    assert_eq!("test", &s);
}
