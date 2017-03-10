pub fn matches() {
    println!("***Matches***");

    match_one();
    matching_on_enums();

    println!("");
}

fn match_one() {
    let x = 3;

    match x {
        1 => println!("one"),
        2 => println!("two"),
        3 => println!("three"),
        4 => println!("four"),
        5 => println!("five"),
        _ => println!("something else"),
    }
}

// enum Message {
//     Quit,
//     ChangeColor(i32, i32, i32),
//     Move { x: i32, y: i32 },
//     Write(String),
// }

// fn quit() { }
// fn change_color(r: i32, g: i32, b: i32) { }
// fn move_cursor(x: i32, y: i32) { }

// fn process_message(msg: Message) {
//     match msg {
//         Message::Quit => quit(),
//         Message::ChangeColor(r, g, b) => change_color(r, g, b),
//         Message::Move { x, y: new_y } => move_cursor(x, new_y),
//         Message::Write(s) => println!("{}", s),
//     }
// }

fn matching_on_enums() {
    // process_message(Message::Quit);
}
