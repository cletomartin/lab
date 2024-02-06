#[derive(Debug)]
#[derive(PartialEq)]
#[derive(Default)]
struct Rectangle {
    width: u32,
    height: u32
}


fn area(rec: &Rectangle) -> u32 {
    rec.width * rec.height
}

fn main() {
    let rec1 = Rectangle { width:10, height: 20 };
    let area = area(&rec1);

    // Debug trait
    println!("rec1 area is {area}. rec1 is {:?}", rec1);
    println!("rec1 area is {area}. rec1 is {:#?}", rec1);
    dbg!(&rec1);

    // PartialEq trait
    let rec2 = Rectangle { width:10, height: 20 };
    println!("rec1 == rec2: {}", rec1 == rec2);

    // Default trait
    let rec3 = Rectangle::default();
    println!("rec3 is: {:?}", rec3);
}
