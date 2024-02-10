pub fn fib_rec(number: u128) -> u128 {
    if number == 0 {
        0
    } else if number <= 2 {
        1
    } else {
        fib_rec(number - 2) + fib_rec(number - 1)
    }
}


pub fn fib_iter(number: u128) -> u128 {
    if number == 0 {
        0
    } else if number <= 2 {
        1
    } else {
        let mut a = 1;
        let mut b = 1;
        let mut fib = 0;
        for _ in 3..=number {
            fib = a + b;
            (a, b) = (b, fib);
        }
        fib
    }
}


#[cfg(test)]
mod tests {
    use super::fib_rec;
    use super::fib_iter;

    #[test]
    fn test_rec() {
        assert_eq!(fib_rec(1), 1);
        assert_eq!(fib_rec(0), 0);
        assert_eq!(fib_rec(4), 3);
        assert_eq!(fib_rec(10), 55);
    }

    #[test]
    fn test_iter() {
        assert_eq!(fib_iter(1), 1);
        assert_eq!(fib_iter(0), 0);
        assert_eq!(fib_iter(4), 3);
        assert_eq!(fib_iter(10), 55);
    }
}
