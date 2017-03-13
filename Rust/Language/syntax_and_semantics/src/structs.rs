pub fn structs() {
    println!("***Structs***", );

    structs_one();
    structs_two();
    tuple_structs();
    newtype_tuples();
    unit_like_structs();

    println!("");
}

struct Point {
    x: i32,
    y: i32,
}

struct PointRef<'a> {
    x: &'a mut i32,
    y: &'a mut i32,
}

// #[derive(Debug)]
// struct Person<'a> {
//     name: &'a str,
//     age: u8
// }

fn structs_one() {
    let origin = Point { x: 2, y: 3 };
    println!("The origin is at ({},{})", origin.x, origin.y);
    assert_eq!(2, origin.x);
    assert_eq!(3, origin.y);

    let mut point = Point { x: 1, y: 4 };
    assert_eq!(1, point.x);
    assert_eq!(4, point.y);

    point.x = 8;
    assert_eq!(8, point.x);
    assert_eq!(4, point.y);

    let point = point; // 'point' is now immutable
    assert_eq!(8, point.x);
    assert_eq!(4, point.y);

    let mut point2 = Point { x: 0, y: 0 };
    {
        let r = PointRef { x: &mut point2.x, y: &mut point2.y };
        *r.x = 5;
        *r.y = 6;
    }
    assert_eq!(5, point2.x);
    assert_eq!(6, point2.y);

    // Shorthand is still unstable
    // let name = "Peter";
    // let age = 28;
    // let peter = Person { name, age };

    // Debug-print string
    // println!("{:?}", peter);

}

struct Point3D {
    x: i32,
    y: i32,
    z: i32,
}

fn structs_two() {
    let origin = Point3D { x: 0, y: 0, z: 0 };
    let point = Point3D { z: 1, x: 2, .. origin };

    assert_eq!(0, point.y);
    assert_eq!(2, point.x);
    assert_eq!(1, point.z);
}

struct Color(i32, i32, i32);
struct PointT(i32, i32, i32);

fn tuple_structs() {
    let black = Color(0, 0, 0);
    let origin = PointT(0, 0, 0);

    let black_r = black.0;
    let PointT(_, origin_y, origin_z) = origin;
    assert_eq!(0, origin_y);
    assert_eq!(0, origin_z);
    assert_eq!(0, black_r);
}

struct Inches(i32);

fn newtype_tuples() {
    let len = Inches(10);
    let Inches(int_len) = len;

    // Same as above
    // let int_len = len.0;

    assert_eq!(10, int_len);
    println!("length is {} inches", int_len);
}

// // Same result
// struct Electron {}
// struct Proton;

fn unit_like_structs() {
    // use the same notation when creating an instance
    // let x = Electron {};
    // let y = Proton;
    // let z = Electron;
}
