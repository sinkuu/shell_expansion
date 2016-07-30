// FIXME: remove this
#![allow(dead_code)]

use std::cmp::{Eq, PartialEq};

#[derive(Debug, Copy, Clone)]
pub struct PatternError;

#[derive(Debug, Clone)]
pub struct Pattern(Vec<Matcher>);

impl Pattern {
    pub fn new(pattern: &str) -> Result<Pattern, PatternError> {
        Ok(Pattern(try!(Pattern::parse(pattern))))
    }

    fn parse(pattern: &str) -> Result<Vec<Matcher>, PatternError> {
        let mut matchers = vec![];

        let mut m = Matcher::Nop;
        let mut cs = pattern.chars();
        loop {
            m = match (m, cs.next()) {
                (m, None) => {
                    matchers.push(m);
                    return Ok(matchers);
                }

                (m, Some('[')) => {
                    let mut inner = vec![];
                    let mut m = m;
                    'bracket: loop {
                        match cs.next() {
                            None => {
                                return Err(PatternError);
                            }

                            Some(']') => {
                                let (nm, done) =
                                    m.combine_seq(Matcher::Group(GroupMatcher::new(inner)));
                                if let Some(done) = done {
                                    matchers.push(done);
                                }
                                m = nm;
                                break 'bracket;
                            }

                            Some('\\') => {
                                match cs.next() {
                                    Some(']') => inner.push(']'),
                                    Some('\\') => inner.push('\\'),
                                    Some(c) => inner.push(c),
                                    None => {
                                        return Err(PatternError);
                                    }
                                }
                            }

                            Some(c) => {
                                inner.push(c);
                            }
                        }
                    }

                    m
                }

                (m, Some('*')) => {
                    let (new_m, done) = m.combine_seq(Matcher::ManyChar);
                    if let Some(done) = done {
                        matchers.push(done);
                    }

                    new_m
                }

                (m, Some('?')) => {
                    let (new_m, done) = m.combine_seq(Matcher::AnyChar);
                    if let Some(done) = done {
                        matchers.push(done);
                    }

                    new_m
                }

                (m, Some('\\')) => {
                    match cs.next() {
                        None => {
                            matchers.push(m);
                            Matcher::Nop
                        }

                        Some(c) => {
                            match c {
                                '[' | '*' | '?' => {
                                    let (new_m, done) = m.combine_seq(Matcher::Char(c));
                                    if let Some(done) = done {
                                        matchers.push(done);
                                    }

                                    new_m
                                }

                                _ => return Err(PatternError),
                            }
                        }
                    }
                }

                (m, Some(c)) => {
                    let (new_m, done) = m.combine_seq(Matcher::Char(c));
                    if let Some(done) = done {
                        matchers.push(done);
                    }

                    new_m
                }
            }
        }
    }

    pub fn matches(&self, len: MatchLength, pos: MatchPosition, s: &str) -> Option<usize> {
        unimplemented!()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchLength {
    Shortest,
    Longest,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchPosition {
    Prefix,
    Suffix,
}

impl PartialEq<Pattern> for Pattern {
    #[inline]
    fn eq(&self, other: &Pattern) -> bool {
        self.0 == other.0
    }
}

impl Eq for Pattern {}

#[derive(Debug, Clone, PartialEq, Eq)]
struct GroupMatcher {
    target: Vec<GroupMatcherTarget>,
    invert: bool,
}

impl GroupMatcher {
    fn new(inner: Vec<char>) -> GroupMatcher {
        let inv = inner[0] == '!';
        let mut inner = if inv { &inner[1..] } else { &inner };

        let mut ts = vec![];

        if inner.len() >= 1 && inner[0] == '-' {
            ts.push(GroupMatcherTarget::Char('-'));
        }

        while !inner.is_empty() {
            let (ca, cb) = (inner[0], inner.get(1));

            inner = &inner[1..];

            match (ca, cb) {
                (c, None) => {
                    ts.push(GroupMatcherTarget::Char(c));
                }

                (c, Some(&'-')) => {
                    if inner.len() >= 2 {
                        ts.push(GroupMatcherTarget::CharBetween(c, inner[1]));
                        inner = &inner[2..];
                    } else {
                        ts.push(GroupMatcherTarget::Char(c));
                        ts.push(GroupMatcherTarget::Char('-'));
                        inner = &inner[1..];
                    }
                }

                (ca, Some(_)) => {
                    ts.push(GroupMatcherTarget::Char(ca));
                }
            }
        }

        ts.sort();
        ts.dedup();

        GroupMatcher {
            target: ts,
            invert: inv,
        }
    }

    fn match_char(&self, input: char) -> bool {
        for v in self.target.iter() {
            match *v {
                GroupMatcherTarget::Char(c) => {
                    if c == input {
                        return !self.invert;
                    }
                }

                GroupMatcherTarget::CharBetween(c_beg, c_end) => {
                    if c_beg <= input && input <= c_end {
                        return !self.invert;
                    }
                }
            }
        }

        self.invert
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum GroupMatcherTarget {
    Char(char),
    CharBetween(char, char),
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Matcher {
    Nop,
    Str(String),
    Char(char),
    Group(GroupMatcher),
    AnyChar,
    ManyChar,
}

impl Matcher {
    fn combine_seq(self, other: Matcher) -> (Matcher, Option<Matcher>) {
        match (self, other) {
            (Matcher::Nop, other) => (other, None),

            (Matcher::Str(s1), Matcher::Str(s2)) => {
                let mut s1 = s1;
                s1.push_str(&s2);
                (Matcher::Str(s1), None)
            }

            (Matcher::Str(s), Matcher::Char(c)) => {
                let mut s = s;
                s.push(c);
                (Matcher::Str(s), None)
            }

            (Matcher::Char(c1), Matcher::Char(c2)) => {
                let mut s = String::with_capacity(2);
                s.push(c1);
                s.push(c2);
                (Matcher::Str(s), None)
            }

            (Matcher::AnyChar, Matcher::AnyChar) => (Matcher::AnyChar, Some(Matcher::AnyChar)),

            (Matcher::ManyChar, Matcher::ManyChar) => (Matcher::ManyChar, None),

            (s, o) => (o, Some(s)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pattern() {
        assert_eq!(Pattern::new("a*b?c[feda-d]").unwrap(),
                   Pattern::new("a**b?c[defa-d]").unwrap());

        let p = Pattern::new("a*b?d[feda-d]").unwrap();
        assert_eq!(p.matches(MatchLength::Shortest, MatchPosition::Prefix, "abcde"), Some(5));

        let p = super::Pattern::new("a*b").unwrap();
        assert_eq!(p.matches(MatchLength::Shortest, MatchPosition::Prefix, "aababc"), Some(3));
        assert_eq!(p.matches(MatchLength::Shortest, MatchPosition::Suffix, "aababc"), None);
        assert_eq!(p.matches(MatchLength::Shortest, MatchPosition::Suffix, "aabab"), Some(2));
        assert_eq!(p.matches(MatchLength::Longest, MatchPosition::Prefix, "aababc"), Some(5));
        assert_eq!(p.matches(MatchLength::Longest, MatchPosition::Suffix, "aababc"), None);
        assert_eq!(p.matches(MatchLength::Longest, MatchPosition::Suffix, "aabab"), Some(5));
    }
}
