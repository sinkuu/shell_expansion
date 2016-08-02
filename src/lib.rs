//! Shell-ish parameter expansion.

use std::collections::HashMap;

mod glob;

use glob::{Pattern, MatchLength, MatchPosition};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpanderError {
    ClosingBracketNotFound,
    ParameterNotSet(Option<String>),
    UnknownEscapeSequence,
    GlobError,
    UnknownPattern,
}

impl From<glob::GlobError> for ExpanderError {
    fn from(_: glob::GlobError) -> Self {
        ExpanderError::GlobError
    }
}

/// Represents parameter formatting components.
/// See See http://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_06_02
/// for specification. (Note: this library doesn't completely follow the spec for now.)
#[derive(Debug, Clone, PartialEq, Eq)]
enum Format {
    PlainStr(String),
    PlainChar(char),
    /// `${parameter}` or `$parameter`: Substitute `parameter`
    Parameter(String),
    /// `${parameter:-word}` or `${parameter-word}`: If `parameter` is unset,
    /// the expansion of `word` is substituted.
    ///
    /// The first `bool` field indicates the existence of a colon.
    /// If the colon is used, null values are treated as unset.
    /// The same shall apply to bool fields below.
    UseDefaultValue(bool, String, Expander),
    /// `${parameter=word}`: If `parameter` is unset,
    /// the expansion of `word` is assigned to `parameter`. In all cases
    /// the value of `parameter` is substituted.
    AssignDefaultValue(bool, String, Expander),
    /// `${parameter:?[word]}`, `${parameter?[word]}`: If `paramter` is unset,
    /// `Expander::expand` returns `ExpanderError::ParameterNotSet` with optional expanded `word`.
    IndicateErrorIfUnset(bool, String, Option<Expander>),
    /// `${parameter+word}`: If `paramter` is unset,
    /// nothing is substituted; otherwise, the expansion of word is substituted.
    UseAlternativeValue(bool, String, Expander),
    /// `${#parameter}` or `$#parameter`
    StringLength(String),
    /// `${parameter%pat}`, `${parameter%%pat}`, `${parameter#pat}`, `${parameter##pat}`
    RemovePattern(String, Expander, MatchLength, MatchPosition),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Expander {
    formats: Vec<Format>,
}

impl Expander {
    /// Compiles the expansion format string.
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

                        let mut rem = s.char_indices().peekable();
                        let mut level = 1;
                        while level > 0 {
                            match rem.next() {
                                None => {
                                    return Err(ExpanderError::ClosingBracketNotFound);
                                }

                                Some((_, '\\')) => {
                                    match rem.next() {
                                        None => return Err(ExpanderError::ClosingBracketNotFound),
                                        Some((_, '}')) => (),
                                        Some((_, '$')) => {
                                            match rem.peek() {
                                                Some(&(_, '}')) | None => (),
                                                Some(&(_, _)) => {
                                                    rem.next().unwrap();
                                                }
                                            }
                                        }
                                        Some(_) => return Err(ExpanderError::UnknownEscapeSequence),
                                    }
                                }

                                Some((_, '$')) => {
                                    match rem.next() {
                                        None => return Err(ExpanderError::ClosingBracketNotFound),
                                        Some((_, '{')) => level += 1,
                                        Some(_) => (),
                                    }
                                }

                                Some((i, '}')) => {
                                    level -= 1;
                                    closing = i;
                                }

                                Some(_) => (),
                            }
                        }

                        let inner = (&s[0..closing]).replace("\\}", "}");
                        s = &s[closing + 1..];

                        let after_ident = inner.find(|c: char| !c.is_alphanumeric());
                        if inner.starts_with('#') {
                            let param = &inner[1..];
                            fmts.push(Format::StringLength(param.to_string()));
                        } else if let Some(after_ident) = after_ident {
                            let fs = (&inner[after_ident..]).chars().next().unwrap();
                            if [':', '-', '+', '?', '='].contains(&fs) {
                                let treat_null_as_unset = fs == ':';

                                let opt = &inner[after_ident..];
                                let opt = if treat_null_as_unset {
                                    &inner[after_ident + 1..]
                                } else {
                                    opt
                                };

                                let param = (&inner[..after_ident]).to_string();
                                let word = &opt[1..];

                                fmts.push(if opt.starts_with('-') {
                                    // use default value
                                    Format::UseDefaultValue(treat_null_as_unset,
                                                            param,
                                                            try!(Expander::new(word)))
                                } else if opt.starts_with('=') {
                                    // assign default value
                                    Format::AssignDefaultValue(treat_null_as_unset,
                                                               param,
                                                               try!(Expander::new(word)))
                                } else if opt.starts_with('+') {
                                    // use alternative value
                                    Format::UseAlternativeValue(treat_null_as_unset,
                                                                param,
                                                                try!(Expander::new(word)))
                                } else if opt.starts_with('?') {
                                    // indicate error if unset
                                    Format::IndicateErrorIfUnset(treat_null_as_unset,
                                                                 param,
                                                                 if word.is_empty() {
                                                                     None
                                                                 } else {
                                                                     Some(try!(Expander::new(word)))
                                                                 })
                                } else {
                                    return Err(ExpanderError::UnknownPattern);
                                })
                            } else if ['#', '%'].contains(&fs) {
                                let param = (&inner[..after_ident]).to_string();
                                let inner = &inner[after_ident..];


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
                                // let pat = try!(Pattern::new(pat, len, pos));
                                fmts.push(Format::RemovePattern(param,
                                                                try!(Expander::new(pat)),
                                                                len,
                                                                pos));
                            }
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

    /// Expands the expansion format string using `params` as parameters.
    pub fn expand(&self, params: &mut HashMap<String, String>) -> Result<String, ExpanderError> {
        let mut res = String::new();

        #[inline]
        fn filter_option<T, F: Fn(&T) -> bool>(o: Option<T>, f: F) -> Option<T> {
            o.and_then(|val| if f(&val) { Some(val) } else { None })
        }

        for f in &self.formats {
            match *f {
                Format::PlainChar(c) => res.push(c),
                Format::PlainStr(ref s) => res.push_str(s),

                Format::Parameter(ref param) => {
                    if let Some(v) = params.get(param) {
                        res.push_str(v);
                    }
                }

                Format::UseDefaultValue(treat_null_as_unset, ref param, ref default) => {
                    // FIXME: fix ugly code after non-lexical borrow lands rust
                    if params.contains_key(param) &&
                       !(treat_null_as_unset && params.get(param).unwrap().is_empty()) {
                        res.push_str(params.get(param).unwrap());
                    } else {
                        res.push_str(&try!(default.expand(params)));
                    }
                }

                Format::AssignDefaultValue(treat_null_as_unset, ref param, ref default) => {
                    // FIXME: ditto
                    if params.contains_key(param) &&
                       !(treat_null_as_unset && params.get(param).unwrap().is_empty()) {
                        res.push_str(params.get(param).unwrap());
                    } else {
                        let e = try!(default.expand(params));
                        res.push_str(&e);
                        params.insert(param.clone(), e);
                    }
                }

                Format::IndicateErrorIfUnset(treat_null_as_unset, ref param, ref error) => {
                    // FIXME: ditto
                    if params.contains_key(param) &&
                       !(treat_null_as_unset && params.get(param).unwrap().is_empty()) {
                        res.push_str(params.get(param).unwrap());
                    } else {
                        let msg = error.clone().map(|e| e.expand(params));
                        let msg = match msg {
                            Some(Err(e)) => return Err(e.into()),
                            Some(Ok(e)) => Some(e),
                            None => None,
                        };
                        return Err(ExpanderError::ParameterNotSet(msg));
                    }
                }

                Format::UseAlternativeValue(treat_null_as_unset, ref param, ref alt) => {
                    if let Some(_) = filter_option(params.get(param),
                                                   |v| !treat_null_as_unset || !v.is_empty()) {
                        res.push_str(&try!(alt.expand(params)));
                    }
                }

                Format::StringLength(ref param) => {
                    if let Some(v) = params.get(param) {
                        res.push_str(&v.chars().count().to_string());
                    }
                }

                Format::RemovePattern(ref param, ref pat, len, pos) => {
                    let e = try!(pat.expand(params));
                    if let Some(v) = params.get(param) {
                        let pat = try!(Pattern::new(&e, len, pos));
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


        let e = Expander::new("$/$nonexistent $HOME/foo\\$bar ${subsub:-${HOME}}").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(),
                   "$/ /home/blah/foo$bar /home/blah");

        let e = Expander::new("${file%.c}.rs").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "example.rs");

        let e = Expander::new("test.${file##*.}").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "test.c");

        params.insert("num".to_string(), "1234５６７８".to_string());
        let e = Expander::new("num is ${#num} chars long, num is $#num chars long! $#num").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(),
                   "num is 8 chars long, num is 8 chars long! 8");

        // NOTE: escapes are not consistent with shell
        let e = Expander::new(r#"${nonexistent-\}\${}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "}${");

        let e = Expander::new(r#"${nonexistent-\${foo\}bar\}baz}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "${foo}bar}baz");
    }

    #[test]
    fn test_default() {
        let mut params = HashMap::new();
        params.insert("foo".to_string(), "value".to_string());

        let e = Expander::new("${bar:-a\\}b}/${bar-c\\}}").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "a}b/c}");

        let e = Expander::new("${foo:-no} ${bar-substituted} ${baz:=assign}").unwrap();
        assert!(!params.contains_key("bar"));
        assert_eq!(e.expand(&mut params).unwrap(), "value substituted assign");
        assert_eq!(params.get("baz").unwrap(), "assign");

        let e = Expander::new("${qux:?}").unwrap().expand(&mut params);
        assert_eq!(e, Err(super::ExpanderError::ParameterNotSet(None)));

        let e = Expander::new("${qux:?${nonexistent:-error text}}").unwrap().expand(&mut params);
        assert_eq!(e,
                   Err(super::ExpanderError::ParameterNotSet(Some("error text".into()))));

        let e = Expander::new("${qux+alt}").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "");
        params.insert("qux".to_string(), "quux".to_string());
        let e = Expander::new("${qux+alt}").unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "alt");
    }

    #[test]
    fn test_glob() {
        let mut params = HashMap::new();
        params.insert("foo".to_string(), "x.blah.c".to_string());
        params.insert("path".to_string(), "foo/bar/baz".to_string());

        let e = Expander::new(r#"${foo#[!a-wy-z][!]}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "blah.c");

        let e = Expander::new(r#"${foo#x.*}/${foo##x.*}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "blah.c/");

        let e = Expander::new(r#"${foo%*.c}/${foo%%*.c}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "x.blah/");

        let e = Expander::new(r#"${path%%/*}:${path##*/}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "foo:baz");

        let e = Expander::new(r#"${path%*${nonexistent:-${path#f?o?}}*}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "foo/");

        params.insert("utf".to_string(), "試験文字列".to_string());
        let e = Expander::new(r#"${utf#??}${utf2=代入}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "文字列代入");
        assert_eq!(params.get("utf2"), Some(&"代入".to_string()));
    }

    // See http://pubs.opengroup.org/onlinepubs/009604499/utilities/xcu_chap02.html#tag_02_06_02
    // for these clutters.
    #[test]
    fn test_null_unset() {
        let mut params = HashMap::new();
        params.insert("foo".to_string(), "test".to_string());
        params.insert("bar".to_string(), "".to_string());

        let e = Expander::new(r#"${foo:-a}/${foo-a}/${bar:-a}/${bar-a}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "test/test/a/");

        let e = Expander::new(r#"${qux:=a}${qux=b}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "aa");
        assert_eq!(params.get("qux"), Some(&"a".into()));

        let e = Expander::new(r#"${bar?a}${bar:?b}"#).unwrap();
        assert_eq!(e.expand(&mut params),
                   Err(ExpanderError::ParameterNotSet(Some("b".into()))));

        let e = Expander::new(r#"${bar:+a}${bar+b}"#).unwrap();
        assert_eq!(e.expand(&mut params).unwrap(), "b");
    }
}
