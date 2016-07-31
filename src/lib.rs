//! Shell-like parameter expansion.

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


#[derive(Debug, Clone, PartialEq, Eq)]
enum Format {
    PlainStr(String),
    PlainChar(char),
    /// `${parameter}` or `$parameter`
    Parameter(String),
    /// `${parameter-word}`
    UseDefaultValue(String, String),
    /// `${parameter=word}`
    AssignDefaultValue(String, String),
    /// `${parameter?word}`
    IndicateErrorIfUnset(String),
    /// `${parameter+word}`
    UseAlternativeValue(String, String),
    /// `${#parameter}` or `$#parameter`
    StringLength(String),
    /// `${parameter%pat}`, `${parameter%%pat}`, `${parameter#pat}`, `${parameter##pat}`
    RemovePattern(String, Pattern),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expander {
    formats: Vec<Format>,
}

impl Expander {
    pub fn new(s: &str) -> Result<Expander, ExpanderError> {
        let mut fmts = vec![];

        let mut s = s;

        'outer: while !s.is_empty() {
            match (s.find("\\$"), s.find('$')) {
                (_, None) => {
                    fmts.push(Format::PlainStr(s.to_string()));
                    break 'outer;
                }
                (escaped_dollar, Some(idx_dollar)) => {
                    if let Some(idx_escaped_dollar) = escaped_dollar {
                        if idx_escaped_dollar + 1 == idx_dollar {
                            let mut string = (&s[0..idx_escaped_dollar]).to_string();
                            string.push('$');
                            fmts.push(Format::PlainStr(string));
                            s = &s[idx_dollar + 1..];
                            continue 'outer;
                        }
                    }

                    fmts.push(Format::PlainStr((&s[0..idx_dollar]).to_string()));
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
                            let param = &inner[1..];
                            fmts.push(Format::StringLength(param.to_string()));
                        } else if let Some(idx) =
                                      inner.find(|c: char| [':', '-', '+', '?'].contains(&c)) {
                            let param = (&inner[..idx]).to_string();

                            let opt = &inner[idx..];
                            // ignoreing ':'
                            let opt = if opt.starts_with(':') {
                                &inner[idx + 1..]
                            } else {
                                opt
                            };

                            let word = (&opt[1..]).to_string();

                            if opt.starts_with('-') {
                                // use default value
                                fmts.push(Format::UseDefaultValue(param, word))
                            } else if opt.starts_with('=') {
                                // assign default value
                                fmts.push(Format::AssignDefaultValue(param, word));
                            } else if opt.starts_with('+') {
                                // use alternative value
                                fmts.push(Format::UseAlternativeValue(param, word));
                            } else if opt.starts_with('?') {
                                // indicate error if unset
                                fmts.push(Format::IndicateErrorIfUnset(param));
                            } else {
                                return Err(ExpanderError::UnknownPattern);
                            }
                        } else if let Some(idx) = inner.find(|c: char| ['#', '%'].contains(&c)) {
                            let param = (&inner[..idx]).to_string();
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
                            fmts.push(Format::RemovePattern(param, pat));
                        } else {
                            if inner.chars().all(|c| c.is_alphanumeric()) {
                                fmts.push(Format::Parameter(inner.as_str().to_string()));
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
                            fmts.push(Format::PlainChar('$'));
                            continue;
                        }

                        let v = (&s[0..e]).to_string();
                        if put_len {
                            fmts.push(Format::StringLength(v))
                        } else {
                            fmts.push(Format::Parameter(v));
                        }

                        s = &s[e..];
                    }
                }
            }
        }

        Ok(Expander { formats: fmts })
    }

    pub fn expand(&self, params: &mut HashMap<String, String>) -> Result<String, ExpanderError> {
        let mut res = String::new();

        for f in &self.formats {
            match *f {
                Format::PlainChar(c) => res.push(c),
                Format::PlainStr(ref s) => res.push_str(s),

                Format::Parameter(ref param) => {
                    if let Some(v) = params.get(param) {
                        res.push_str(v);
                    }
                }

                Format::UseDefaultValue(ref param, ref default) => {
                    if let Some(v) = params.get(param) {
                        res.push_str(v);
                    } else {
                        res.push_str(default);
                    }
                }

                Format::AssignDefaultValue(ref param, ref default) => {
                    res.push_str(params.entry(param.clone())
                        .or_insert(default.clone()));
                }

                Format::IndicateErrorIfUnset(ref param) => {
                    if let Some(v) = params.get(param) {
                        res.push_str(v);
                    } else {
                        return Err(ExpanderError::VariableNotFound);
                    }
                }

                Format::UseAlternativeValue(ref param, ref alt) => {
                    if let None = params.get(param) {
                        res.push_str(alt);
                    }
                }

                Format::StringLength(ref param) => {
                    if let Some(v) = params.get(param) {
                        res.push_str(&v.chars().count().to_string());
                    }
                }

                Format::RemovePattern(ref param, ref pat) => {
                    if let Some(v) = params.get(param) {
                        if let Some(i) = pat.matches(v) {
                            match pat.pos {
                                MatchPosition::Prefix => {
                                    res.push_str(&v[i..]);
                                }

                                MatchPosition::Suffix => {
                                    res.push_str(&v[..v.len() - i]);
                                }
                            }
                        } else {
                            res.push_str(v);
                        }
                    }
                }
            }
        }

        Ok(res)
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;
    use super::*;

    #[test]
    fn it_works() {
        let mut params = HashMap::new();
        params.insert("HOME".to_string(), "/home/blah".to_string());
        params.insert("file".to_string(), "example.c".to_string());


        let e = Expander::new("/$nonexistent $HOME/foo\\$bar").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "/ /home/blah/foo$bar");

        let e = Expander::new("${file%.c}.rs").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "example.rs");

        let e = Expander::new("test.${file##*.}").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "test.c");

        params.insert("num".to_string(), "1234５６７８".to_string());

        let e = Expander::new("num is ${#num} chars long, num is $#num chars long!").unwrap();
        println!("{:?}", e);
        assert_eq!(e.expand(&mut params).unwrap(),
                   "num is 8 chars long, num is 8 chars long!")
    }

    #[test]
    fn test_default() {
        let mut params = HashMap::new();
        params.insert("foo".to_string(), "value".to_string());

        let e = Expander::new("${bar:-a\\}b}/${bar:-c\\}}").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "a}b/c}");

        let e = Expander::new("${foo:-no} ${bar:-substituted} ${baz:=assign}").unwrap();
        assert!(!params.contains_key("bar"));
        assert_eq!(e.expand(&mut params).unwrap(), "value substituted assign");
        assert_eq!(params.get("baz").unwrap(), "assign");

        let e = Expander::new("${qux:?}").unwrap().expand(&mut params);
        assert_eq!(e, Err(super::ExpanderError::VariableNotFound));
    }

    #[test]
    fn test_glob() {
        let mut params = HashMap::new();
        params.insert("foo".to_string(), "x.blah.c".to_string());
        params.insert("path".to_string(), "foo/bar/baz".to_string());

        let e = Expander::new(r#"${foo#x.*}/${foo##x.*}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "blah.c/");

        let e = Expander::new(r#"${foo%*.c}/${foo%%*.c}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "x.blah/");

        let e = Expander::new(r#"${path%%/*}:${path##*/}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "foo:baz");
    }
}
