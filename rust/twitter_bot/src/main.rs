extern crate twapi;
extern crate restson;
extern crate serde_json;
extern crate chrono;

#[macro_use]
extern crate serde_derive;

use std::{
    env,
    io,
    str::FromStr,
};


#[derive(Debug, Serialize, Deserialize)]
struct Favorite {
    id_str: String,
    created_at: DateTime<FixedOffset>,
    text: String,
    // user: Value,
}


use twapi::Twapi;

use serde_json::{
    Value,
    ser,
};

use chrono::prelude::*;


fn authenticate() -> io::Result<twapi::UserAuth> {

    let consumer_key_var = "DETWEEWTED_CONSUMER_KEY";
    let consumer_secret_var = "DETWEEWTED_CONSUMER_SECRET_KEY";
    let token_var = "DETWEEWTED_TOKEN";
    let token_secret_var = "DETWEEWTED_TOKEN_SECRET";

    let auth = twapi::UserAuth::new(
        &env::var(&consumer_key_var).expect(consumer_key_var),
        &env::var(&consumer_secret_var).expect(consumer_secret_var),
        &env::var(&token_var).expect(token_var),
        &env::var(&token_secret_var).expect(token_secret_var)
    );
    // let token = twapi::oauth2::get_bearer_token(&key, &secret).unwrap(); // FIXME
    // let auth = twapi::ApplicationAuth::new(&token);

    Ok(auth)

}


fn main() {

    let twitter = authenticate().unwrap(); // FIXME

    let res: Value = twitter.get("https://api.twitter.com/1.1/favorites/list.json",
                          &vec![]
                          // &vec![("screen_name", "badmutex")]
    ).unwrap().json().unwrap();
    // println!("{}", ser::to_string_pretty(&res).unwrap());

    let favs: Vec<Favorite> = serde_json::from_value(res).unwrap();
    favs.iter().for_each(|f|{
        println!("{:?}", f);
    });

    // let t = "Fri Dec 22 16:48:54 +0000 2017";
    // let t = DateTime::parse_from_str(t, "%a %b %d %T %z %Y");
    // println!("{:?}", t);

}
