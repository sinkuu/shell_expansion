//! Shell-like variable expansion.

use std::collections::HashMap;

mod glob;

use glob::{Pattern, MatchLength, MatchPosition};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ExpanderError {
    ClosingBracketNotFound,
    VariableNotFound,
    UnknownEscapeSequence,
    GlobError,
    UnknownPattern,
}

impl From<glob::GlobError> for ExpanderError {
    fn from(_: glob::GlobError) -> Self {
        ExpanderError::GlobError
    }
}

pub fn expand(mut s: &str, vars: &mut HashMap<String, String>) -> Result<String, ExpanderError> {
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

                    let inner = (&s[0..closing]).replace("\\}", "}");
                    s = &s[closing + 1..];

                    if inner.starts_with('#') {
                        let var = &inner[1..];

                        match vars.get(var) {
                            Some(v) => res.push_str(&v.chars().count().to_string()),
                            None => {
                                return Err(ExpanderError::VariableNotFound);
                            }
                        };
                    } else if let Some(idx) =
                                  inner.find(|c: char| [':', '-', '+', '?'].contains(&c)) {
                        let varname = &inner[..idx];

                        let opt = &inner[idx..];
                        // ignoreing ':'
                        let opt = if opt.starts_with(':') {
                            &inner[idx + 1..]
                        } else {
                            opt
                        };

                        if opt.starts_with('-') {
                            // use default value
                            match vars.get(varname) {
                                Some(v) => res.push_str(v),
                                None => res.push_str(&opt[1..]),
                            }
                        } else if opt.starts_with('=') {
                            // assign default value
                            res.push_str(vars.entry(varname.to_string())
                                .or_insert((&opt[1..]).to_string()));
                        } else if opt.starts_with('+') {
                            // use alternative value
                            match vars.get(varname) {
                                Some(_) => (),
                                None => res.push_str(&inner[idx + 2..]),
                            }
                        } else if opt.starts_with('?') {
                            // indicate error if unset
                            match vars.get(varname) {
                                Some(v) => res.push_str(v),
                                None => {
                                    return Err(ExpanderError::VariableNotFound);
                                }
                            }
                        } else {
                            return Err(ExpanderError::UnknownPattern);
                        }
                    } else if let Some(idx) = inner.find(|c: char| ['#', '%'].contains(&c)) {
                        let var = if let Some(v) = vars.get(&inner[..idx]) {
                            v
                        } else {
                            continue 'outer;
                        };

                        let inner = &inner[idx..];

                        let (pat, len, pos) = if inner.starts_with("##") {
                            (&inner[2..], MatchLength::Longest, MatchPosition::Prefix)
                        } else if inner.starts_with('#') {
                            (&inner[1..], MatchLength::Shortest, MatchPosition::Prefix)
                        } else if inner.starts_with("%%") {
                            (&inner[2..], MatchLength::Longest, MatchPosition::Suffix)
                        } else if inner.starts_with('%') {
                            (&inner[1..], MatchLength::Shortest, MatchPosition::Suffix)
                        } else {
                            return Err(ExpanderError::UnknownPattern);
                        };
                        let pat = try!(Pattern::new(pat, len, pos));

                        if let Some(i) = pat.matches(var) {
                            match pos {
                                MatchPosition::Prefix => {
                                    res.push_str(&var[i..]);
                                }

                                MatchPosition::Suffix => {
                                    res.push_str(&var[..var.len() - i]);
                                }
                            }
                        } else {
                            res.push_str(var);
                        }
                    } else {
                        if inner.chars().all(|c| c.is_alphanumeric()) {
                            if let Some(v) = vars.get(inner.as_str()) {
                                res.push_str(v);
                            }
                        } else {
                            return Err(ExpanderError::UnknownPattern);
                        }
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

                    if let Some(v) = vars.get(&s[0..e]) {
                        if put_len {
                            res.push_str(&v.chars().count().to_string());
                        } else {
                            res.push_str(v);
                        }
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
    use std::collections::HashMap;

    #[test]
    fn it_works() {
        let mut vars = HashMap::new();
        vars.insert("HOME".to_string(), "/home/blah".to_string());

        let e = super::expand("/$nonexistent $HOME/foo\\$bar", &mut vars);
        assert_eq!(e.unwrap(), "/ /home/blah/foo$bar");

        vars.insert("var".to_string(), "1234５６７８".to_string());

        let e = super::expand("var is ${#var} chars long, yes, var is $#var chars long",
                              &mut vars);
        assert_eq!(e.unwrap(), "var is 8 chars long, yes, var is 8 chars long")
    }

    #[test]
    fn test_default() {
        let mut vars = HashMap::new();
        vars.insert("foo".to_string(), "value".to_string());

        let e = super::expand("${bar:-a\\}b}/${bar:-c\\}}", &mut vars);
        assert_eq!(e.unwrap(), "a}b/c}");

        let e = super::expand("${foo:-no} ${bar:-substituted} ${baz:=assign}", &mut vars);
        assert!(!vars.contains_key("bar"));
        assert_eq!(e.unwrap(), "value substituted assign");
        assert_eq!(vars.get("baz").unwrap(), "assign");

        let e = super::expand("${qux:?}", &mut vars);
        assert_eq!(e, Err(super::ExpanderError::VariableNotFound));
    }

    #[test]
    fn test_glob() {
        let mut vars = HashMap::new();
        vars.insert("foo".to_string(), "x.blah.c".to_string());
        vars.insert("path".to_string(), "foo/bar/baz".to_string());

        let e = super::expand(r#"${foo#x.*}/${foo##x.*}"#, &mut vars);
        assert_eq!(e.unwrap(), "blah.c/");

        let e = super::expand(r#"${foo%*.c}/${foo%%*.c}"#, &mut vars);
        assert_eq!(e.unwrap(), "x.blah/");


        let e = super::expand(r#"${path%%/*}:${path##*/}"#, &mut vars);
        assert_eq!(e.unwrap(), "foo:baz");
    }
}
