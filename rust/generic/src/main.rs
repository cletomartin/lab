use std::cmp::PartialOrd;

// Generic function
fn largest<T: PartialOrd>(list: &[T]) -> &T {
    let mut largest = &list[0];

    for number in list {
        if number > largest {
            largest = number;
        }
    }
    largest
}

#[cfg(test)]
mod tests {
    use super::largest;

    #[test]
    fn test_integers() {
        let v = vec![20, 100, 2];
        assert_eq!(largest(&v), &100);
    }

    #[test]
    fn test_chars() {
        let v = vec!['1', 'z', 'A'];
        assert_eq!(largest(&v), &'z');
    }

    #[test]
    fn test_strings() {
        let v = vec![String::from("Hello"), String::from("Bye")];
        assert_eq!(largest(&v), &"Hello");
    }
}

// Generic struct
struct Point<T, U> {
    x: T,
    y: U,
}

impl<T, U> Point<T, U> {
    fn x(&self) -> &T {
        &self.x
    }

    fn y(&self) -> &U {
        &self.y
    }
}

impl Point<f32, f32> {
    fn distance_from_origin(&self) -> f32 {
        (self.x.powi(2) + self.y.powi(2)).sqrt()
    }
}

fn main() {
    let number_list = vec![20, 12, 100, 84, 5];
    let l = largest(&number_list);
    println!("The largest number is {l}");

    let char_list = vec!['a', 'z', 'b'];
    let l = largest(&char_list);
    println!("The largest number is {l}");

    let p1 = Point { x: 1, y: 2.5 };
    let p2 = Point { x: 1.2, y: -2 };
    let p3 = Point { x: 3.2, y: 23.2 };
    println!("Points: {}/{}, {}/{}", p1.x(), p1.y(), p2.x(), p2.y());
    println!("Distance for p3: {}", p3.distance_from_origin());
}
