use std;

pub fn methods() {
    println!("***Methods***");

    method_calls();
    chaining_method_calls();
    associated_functions();
    builder_pattern();

    println!("");
}

struct Circle {
    x: f64,
    y: f64,
    radius: f64,
}

impl Circle {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.radius * self.radius)
    }
}

#[allow(dead_code)]
impl Circle {
    fn mutable_ref(&mut self) {
        println!("Taking self by mutable reference.");
    }

    fn takes_ownership(self) {
        println!("Taking ownership of self");
    }
}

fn method_calls() {
    let c = Circle { x: 0.0, y: 0.0, radius: 2.0 };
    println!("c: ({},{}) -> area is {}", c.x, c.y, c.area());
}

#[allow(dead_code)]
impl Circle {
    fn grow(&self, increment: f64) -> Circle {
        Circle { x: self.x, y: self.y, radius: self.radius+increment }
    }
}

fn chaining_method_calls() {
    let c = Circle { x: 0.0, y: 0.0, radius: 2.0 };
    println!("{}", c.area());

    let d = c.grow(2.0).area();
    println!("{}", d);
}

#[allow(dead_code)]
impl Circle {
    fn new(x: f64, y: f64, radius: f64) -> Circle {
        Circle {
            x: x,
            y: y,
            radius: radius,
        }
    }
}

#[allow(dead_code, unused_variables)]
fn associated_functions() {
    let c = Circle::new(0.0, 0.0, 2.0);
}

struct Circle2 {
    x: f64,
    y: f64,
    radius: f64,
}

// Builder Pattern

impl Circle2 {
    fn area(&self) -> f64 {
        std::f64::consts::PI * (self.radius * self.radius)
    }
}

struct Circle2Builder {
    x: f64,
    y: f64,
    radius: f64,
}

impl Circle2Builder {
    fn new() -> Circle2Builder {
        Circle2Builder { x: 0.0, y: 0.0, radius: 1.0 }
    }

    fn x(&mut self, coordinate: f64) -> &mut Circle2Builder {
        self.x = coordinate;
        self
    }

    fn y(&mut self, coordinate: f64) -> &mut Circle2Builder {
        self.y = coordinate;
        self
    }

    fn radius(&mut self, radius: f64) -> &mut Circle2Builder {
        self.radius = radius;
        self
    }

    fn finalize(&self) -> Circle2 {
        Circle2 { x: self.x, y: self.y, radius: self.radius }
    }
}

fn builder_pattern() {
    let c = Circle2Builder::new().x(1.0).y(2.0).radius(2.0).finalize();
    let c2: Circle2 = c;

    println!("area: {}", c2.area());
    assert_eq!(c2.x, 1.0);
    assert_eq!(c2.y, 2.0);
}
