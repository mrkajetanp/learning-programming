// pub fn enums() {
//     println!("***Enums***");
//     // enums_one();
//     println!("");
// }

// enum Message {
//     Quit,
//     ChangeColor(i32, i32, i32),
//     Move { x: i32, y: i32 },
//     Write(String),
// }

// enum BoardGameTurn {
//     Move { squares: i32 },
//     Pass,
// }

// fn foo(x: String) -> Message {
//     Message::Write(x)
// }

// fn enums_one() {
//     let x: Message = Message::Move { x: 3, y: 4 };
//     let y: BoardGameTurn = BoardGameTurn::Move { squares: 1 };
//     let m = Message::Write("test str".to_string());
//     let x2 = foo("Hello there".to_string());
//     let v = vec!["Hello".to_string(), "World".to_string()];
//     let v1: Vec<Message> = v.into_iter().map(Message::Write).collect();
// }
