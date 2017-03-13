pub fn drop() {
    println!("***Drop***");

    drop_one();
    drop_two();

    println!("");
}

struct HasDrop;

impl Drop for HasDrop {
    fn drop(&mut self) {
        println!("Dropping!");
    }
}

#[allow(unused_variables)]
fn drop_one() {
    let x = HasDrop;

    // do something

    println!("something with x..");

} // x goes out of scope here

struct Firework {
    strength: i32,
}

impl Drop for Firework {
    fn drop(&mut self) {
        println!("BOOM times {} !!", self.strength);
    }
}

// values are dropped in the opposite order they are declared
#[allow(unused_variables)]
fn drop_two() {
    let firecracker = Firework { strength: 1 };
    let tnt = Firework { strength: 100 };
}
