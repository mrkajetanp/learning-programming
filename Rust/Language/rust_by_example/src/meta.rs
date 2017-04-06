pub fn meta() {
    println!("***Meta***");

    documentation();
    testing();

    println!("");
}

fn documentation() {
    /// A human being representation
    struct Person {
        /// A person must have a name
        name: String,
    }

    impl Person {
        /// Returns a person with the name given them
        ///
        /// # Arguments
        ///
        /// * `name` - A string slice that holds the name of the person
        ///
        /// # Example
        ///
        /// ```
        /// // You can have Rust code inside the comments
        /// // Passing --test to Rustdoc will test it for you
        /// use doc::Person;
        /// let person = Person::new("name");
        /// ```
        fn new(name: &str) -> Person {
            Person {
                name: name.to_string(),
            }
        }

        /// Gives a friendly hello
        ///
        /// Says "Hello [name]" to the Person it is called on
        fn hello(&self) {
            println!("Hello, {}!", self.name);
        }
    }

    let john = Person::new("John");
    john.hello();
}

fn testing() {
    // compile this fn only when the test-suite is *not* being run
    #[cfg(not(test))]
    fn some_main() {
        println!("You're not testing!");
    }

    // compile the module test only when the test-suite is run
    #[cfg(test)]
    mod test {
        // helper function
        fn distance(a: (f32, f32), b: (f32, f32)) -> f32 {
            (
                (b.0 - a.0).powi(2) +
                    (b.1 - a.1).powi(2)
            ).sqrt()
        }
        #[test]
        fn distance_test() {
            assert!(distance((0_f32, 0_f32), (1_f32, 1_f32)) == (2_f32).sqrt());
        }

        #[test]
        #[should_panic]
        fn failing_test() {
            assert!(1_i32 == 2_i32);
        }
    }
}
