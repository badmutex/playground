
extern crate evolve_string;
use evolve_string::charset::random_character;

fn main() {
    for _ in range(1, 50i) {
        print!("{}", random_character(None));
    }
    println!("");
}

