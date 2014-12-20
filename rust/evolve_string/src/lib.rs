
#![allow(dead_code,unused_imports)] // 
#![feature(globs)]


pub mod charset {

    use std::ascii::OwnedAsciiExt;
    use std::option::Option;
    use std::ops::Add;
    use std::rand::{Rng,TaskRng,task_rng};

    static ALPHA: &'static str = "abcdefghijklmnopqrstuvwxyz";

    pub fn alpha_lower() -> String {
        ALPHA.to_string().into_ascii_lower()
    }

    pub fn alpha_upper() -> String {
        ALPHA.to_string().into_ascii_upper()
    }

    pub fn random_character(rng: Option<TaskRng>) -> char {
        let mut my_rng = rng.unwrap_or(task_rng());
        let choices = alpha_lower() + alpha_upper();
        let idx     = my_rng.gen_range(0, choices.len());
        choices.as_slice().char_at(idx)
    }

}


mod ga {

    use std::rand::random;

    use charset;
    
    struct Solution(Vec<char>);

    trait Evolvable {
        fn new(x: String) -> Self;
        fn mutate(&self) -> &Self;
        fn cross(&self, other: &Self) -> &Self;
    }

    impl Evolvable for Solution {
        fn new(s: String) -> Solution {
            Solution(s.as_slice().chars().collect())
        }

        fn mutate(&self) -> &Solution {
            let i: uint = random();
            let c = charset::random_character(None);
            let &Solution(ref v) = self;
            let vm = v.as_mut_slice();
            vm[i] = c;
            Solution(String::from_chars(vm))
        }

        fn cross(&self, other: &Solution) -> &Solution {
            self
        }
    }

}
