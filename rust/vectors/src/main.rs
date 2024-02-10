fn main() {
    let v = vec![1, 2, 3, 4, 5];

    let third: i32 = v[2];
    println!("The third element is {third}");

    match v.get(2) {
        Some(third) => println!("The third element is {third}"),
        None => println!("There is no third element."),
    };

    let names = vec!["Bob", "Frank", "Ferris"];
    for name in names.iter() {
        match *name {
            "Ferris" => println!("There is a rustacean among us!"),
            _ => println!("Hello {}", name),
        }
    }
    println!("names: {:?}", names);

    let mut names = vec!["Bob", "Frank", "Ferris"];
    for name in names.iter_mut() {
        *name = "hello";
    }
    println!("names: {:?}", names);

    let names = vec!["Bob", "Frank", "Ferris"];
    for name in names.into_iter() {
        println!("{name}");
    }
    // println!("names: {:?}", names);  Does not work because into_iter moved the vector ownership inside the loop
}
