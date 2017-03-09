pub fn lifetimes() {
    println!("***Lifetimes***");

    with_functions();
    in_structs();

    println!("");
}

struct Foo<'a> {
    x: &'a i32,
}

impl<'a> Foo<'a> {
    fn xf(&self) -> &'a i32 {
        self.x
    }
}

fn in_structs() {
    let y = &5;
    let f = Foo { x: y };

    println!("f.x is {}", f.x);
    println!("f.xf returns {}", f.xf());

    // let x: &'static str = "Hello, world";
    // static FOO: i32 = 5;
    // let y: &'static i32 = &FOO;
}

fn with_functions() {
    let line = "lang:en=Hello";
    let lang = "en";

    let v;
    {
        let p = format!("lang:{}=", lang);
        v = skip_prefix(line, p.as_str());
        println!("x is '{}'", v);
    }
    // v is out of scope
}

fn skip_prefix<'a, 'b>(line: &'a str, prefix: &'b str) -> &'a str {
    println!("prefix: '{}'", prefix);
    line
}
