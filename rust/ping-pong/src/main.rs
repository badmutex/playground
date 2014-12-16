
extern crate time;

use std::comm::channel;
use std::task::spawn;
use std::rand;
use std::time::duration::Duration;
use std::io::timer::sleep;

fn main() {
    println!("Hello, world!");

    let (tx0, rx0) = channel();
    let (tx1, rx1) = channel();

    // ping
    spawn(move || {
        loop {
            tx0.send("ping".to_string());
            let msg = rx1.recv();
            println!("{} Ping got {}", time::now().asctime(), msg);
            let seconds = rand::random::<i64>() % 10;
            sleep(Duration::seconds(seconds));
        }
    });

    // pong
    spawn(move || {
        loop {
            let msg = rx0.recv();
            println!("{} Pong got {}", time::now().asctime(), msg);
            tx1.send("pong".to_string());
        }
    });
}
