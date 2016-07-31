extern crate shell_expansion;

use shell_expansion::Expander;
use std::collections::HashMap;

fn main() {
    let mut params = HashMap::new();
    params.insert("file".to_string(), "example.c".to_string());

    let e = Expander::new("${file%.c}.rs").unwrap();
    assert_eq!(e.expand(&mut params).unwrap(), "example.rs");

    let e = Expander::new("test.${file##*.}").unwrap();
    assert_eq!(e.expand(&mut params).unwrap(), "test.c");
}
