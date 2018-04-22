extern crate serde;
extern crate serde_json;

#[macro_use]
extern crate serde_derive;

use serde_json::{Value, Error};

#[derive(Debug, Serialize, Deserialize)]
struct Person {
    name: String,
    age: u8,
    phones: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
struct Foo {
    bar: String,
}

fn main() {
    let data = r#"{
                    "name": "John Doe",
                    "age": 43,
                    "phones": [
                      "+44 1234567",
                      "+44 2345678"
                    ],
                    "other": "Aaaaaahhh!"
                  }"#;

    let v: Value = serde_json::from_str(data).unwrap();
    println!("{:?}", v["phones"][0]);

match v.get("key") {
    Some(x) => println!("{}", x),
    None => println!("No key"),
}

    let p: Person = serde_json::from_value(v).unwrap();
    println!("{:?}", p);

    let data = r#"{ "bar": "hello world",
                    "baz": "this is ignored"
                  }"#;
    let foo: Foo = serde_json::from_str(data).unwrap();
    println!("{:?}", foo);

}
