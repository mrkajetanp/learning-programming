pub fn _marker() {
    phantom_data_struct();
    copy_trait();
    // send_trait
    // sync_trait
    // unsize_trait
}

fn phantom_data_struct() {
    use std::marker::PhantomData;

    struct Slice<'a, T: 'a> {
        start: *const T,
        end: *const T,
        phantom: PhantomData<&'a T>,
    }

    fn borrow_vec<'a, T>(vec: &'a Vec<T>) -> Slice<'a, T> {
        let ptr = vec.as_ptr();
        Slice {
            start: ptr,
            end: unsafe { ptr.offset(vec.len() as isize) },
            phantom: PhantomData,
        }
    }
}

fn copy_trait() {
    #[derive(Copy, Clone)]
    struct MyStructOne;

    struct MyStructTwo;

    impl Clone for MyStructTwo {
        fn clone(&self) -> MyStructTwo {
            *self
        }
    }

    impl Copy for MyStructTwo { }
}

fn sized_trait() {
    struct Foo<T>(T);
    struct Bar<T: ?Sized>(T);

    // ?Sized removes implicit sized bound on type parameters

    // struct FooUse(Foo<[i32]>); // error
    struct BarUse(Bar<[i32]>);
}
