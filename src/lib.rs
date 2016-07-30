//! See http://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_06_02
//! for POSIX specifications of shell parameter expansion.

#[derive(Debug, Copy, Clone)]
pub enum ExpanderError {
    ClosingBracketNotFound,
    VariableNotFound,
    UnknownEscapeSequence,
}


pub fn expand<GF: Fn(&str) -> Option<&str>>(mut s: &str, get: GF) -> Result<String, ExpanderError> {
    let mut res = String::with_capacity(s.len());

    'outer: while !s.is_empty() {
        match (s.find("\\$"), s.find('$')) {
            (_, None) => {
                res.push_str(s);
                break 'outer;
            }
            (escaped_dollar, Some(idx_dollar)) => {
                if let Some(idx_escaped_dollar) = escaped_dollar {
                    if idx_escaped_dollar + 1 == idx_dollar {
                        res.push_str(&s[0..idx_escaped_dollar]);
                        res.push('$');
                        s = &s[idx_dollar + 1..];
                        continue 'outer;
                    }
                }

                res.push_str(&s[0..idx_dollar]);
                s = &s[idx_dollar + 1..];

                if s.starts_with('{') {
                    s = &s[1..];

                    let mut closing = if let Some(c) = s.find('}') {
                        c
                    } else {
                        return Err(ExpanderError::ClosingBracketNotFound);
                    };

                    let mut rem = s;
                    while let Some(ec) = rem.find("\\}") {
                        if ec < closing {
                            rem = &s[closing + 1..];
                            if let Some(c) = rem.find('}') {
                                closing += c + 1;
                            } else {
                                return Err(ExpanderError::ClosingBracketNotFound);
                            }
                        } else {
                            break;
                        }
                    }

                    let inner = &s[0..closing].replace("\\}", "}");
                    s = &s[closing + 1..];

                    if inner.starts_with('#') {
                        let var = &inner[1..];

                        match get(var) {
                            Some(v) => res.push_str(&v.chars().count().to_string()),
                            None => {
                                return Err(ExpanderError::VariableNotFound);
                            }
                        };
                    } else {
                        match get(inner) {
                            Some(v) => res.push_str(v),
                            None => {
                                return Err(ExpanderError::VariableNotFound);
                            }
                        };
                    }
                } else {
                    let put_len = if s.starts_with('#') {
                        s = &s[1..];
                        true
                    } else {
                        false
                    };

                    let e = match s.find(|c: char| !c.is_alphanumeric()) {
                        Some(e) => e,
                        None => s.len(),
                    };

                    if e == 0 {
                        res.push('$');
                        continue;
                    }

                    if let Some(v) = get(&s[0..e]) {
                        if put_len {
                            res.push_str(&v.chars().count().to_string());
                        } else {
                            res.push_str(v);
                        }
                    } else {
                        return Err(ExpanderError::VariableNotFound);
                    }

                    s = &s[e..];
                }
            }
        }
    }

    Ok(res)
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let e = super::expand("/path/1 $HOME/foo\\$bar ${a\\}b} ${soumen\\}tabe\\}tai\\}}",
                              |v| {
            match v {
                "HOME" => Some("/home/blah"),
                "a}b" => Some("/path/2"),
                "soumen}tabe}tai}" => Some("/path/3"),
                _ => None,
            }
        });
        assert_eq!(e.unwrap(), "/path/1 /home/blah/foo$bar /path/2 /path/3");

        let e = super::expand("var is ${#var} chars long, yes, var is $#var chars long",
                              |v| {
                                  match v {
                                      "var" => Some("1234５６７８"),
                                      _ => None,
                                  }
                              });
        assert_eq!(e.unwrap(), "var is 8 chars long, yes, var is 8 chars long")
    }
}
