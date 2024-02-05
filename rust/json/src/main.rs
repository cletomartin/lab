use std::fs;

fn main() {
    let data = fs::read_to_string("example.json").expect("Unable to read the file");
    let data = &data;
    let json : serde_json::Value = serde_json::from_str(data).expect("JSON was not well-formatted");
    println!("{json}");
}
