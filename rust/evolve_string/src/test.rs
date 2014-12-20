
use std::ascii::OwnedAsciiExt;

pub struct Header {
    method: String,
    url: String
}

impl Header {
    pub fn new(method: &str, url: &str) -> Header {
        Header {
            method: method.to_string(),
            url: url.to_string()
        }
    }

    pub fn method(&self) -> String {
        self.method.to_string().into_ascii_upper()
    }

    pub fn url(&self) -> String {
        self.url.to_string().into_ascii_lower()
    }
}

