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
                    match get(inner) {
                        Some(v) => res.push_str(v),
                        None => {
                            return Err(ExpanderError::VariableNotFound);
                        }
                    };
                    s = &s[closing + 1..];
                } else {
                    let e = match s.find(|c: char| !c.is_alphanumeric()) {
                        Some(e) => e,
                        None => s.len(),
                    };

                    if e == 0 {
                        res.push('$');
                        continue;
                    }

                    if let Some(v) = get(&s[0..e]) {
                        res.push_str(v);
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
        let e = super::expand("/path/1 $HOME/foo\\$bar ${hoge} ${soumen\\}tabe\\}tai\\}}",
                              |v| {
            match v {
                "HOME" => Some("/home/blah"),
                "hoge" => Some("/path/2"),
                "soumen}tabe}tai}" => Some("/path/3"),
                _ => None,
            }
        });
        assert_eq!(e.unwrap(), "/path/1 /home/blah/foo$bar /path/2 /path/3");
    }
}
