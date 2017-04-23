pub fn _hash() {
    example();
}

fn example() {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    #[derive(Hash)]
    struct Person {
        id: u32,
        name: String,
        phone: u64,
    }

    let person1 = Person { id: 5, name: "Janet".to_string(), phone: 555_666_777 };
    let person2 = Person { id: 5, name: "Bob".to_string(), phone: 555_666_777 };

    assert!(hash(&person1) != hash(&person2));

    fn hash<T: Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }
}

fn implementing_hash() {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    struct Person {
        id: u32,
        name: String,
        phone: u64,
    }

    impl Hash for Person {
        fn hash<H: Hasher>(&self, state: &mut H) {
            self.id.hash(state);
            self.phone.hash(state);
        }
    }

    let person1 = Person { id: 5, name: "Janet".to_string(), phone: 555_666_777 };
    let person2 = Person { id: 5, name: "Bob".to_string(), phone: 555_666_777 };

    assert!(hash(&person1) != hash(&person2));

    fn hash<T: Hash>(t: &T) -> u64 {
        let mut s = DefaultHasher::new();
        t.hash(&mut s);
        s.finish()
    }
}
