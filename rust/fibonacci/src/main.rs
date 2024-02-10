use fibonacci::fib_iter;
use std::io;

fn main() {
    println!("Input the fibonnaci number");

    let number: u32;

    loop {
        let mut user_input = String::new();
        io::stdin()
            .read_line(&mut user_input)
            .expect("failed to read line");
        number = match user_input.trim().parse() {
            Ok(num) => num,
            Err(err) => {
                println!("{err}");
                continue;
            }
        };
        break;
    }
    let result = fib_iter(number.into());
    println!("Result: {result}");
}
