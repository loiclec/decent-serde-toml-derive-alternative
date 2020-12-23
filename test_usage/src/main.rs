
use std::collections::HashMap;

use decent_toml_rs_alternative as toml;

use decent_serde_toml_derive_alternative::{FromToml, ToToml};
use toml::{FromToml, ToToml};

#[derive(Debug, FromToml, ToToml)]
enum X {
    A(u8),
    B { y: Y, hello: u8 },
    C
}

#[derive(Debug, FromToml, ToToml)]
struct Y {
    a: u8,
    b: String,
}

fn main() {
    let toml_input = r#"
    kind = "B"
    [payload.y]
    a = 89
    b = "bye"
    [payload]
    hello = 200
    "#;
    
    let lines = toml::parse_toml_lines(toml_input).unwrap();
    let toml = toml::TomlValue::Table(toml::toml_value_from_lines(lines).unwrap());

    let value = X::from_toml(Some(&toml)).unwrap();
    let toml2 = value.to_toml().unwrap();

    println!("{:?}", toml2);

    let mut toml3 = HashMap::new();
    toml3.insert("root".to_string(), toml2);

    println!("{}", toml::print(&toml3));
}
