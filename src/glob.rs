#[derive(Debug, Copy, Clone)]
pub enum GlobError {
    UnknownEscapeSequence,
    StrayBracket,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern {
    pub len: MatchLength,
    pub pos: MatchPosition,
    matchers: Vec<Matcher>,
}

impl Pattern {
    pub fn new(pattern: &str, len: MatchLength, pos: MatchPosition) -> Result<Pattern, GlobError> {
        let mut m = try!(Pattern::parse(pattern));
        if pos == MatchPosition::Suffix {
            m = m.into_iter().map(|x| x.reverse()).rev().collect();
        }

        Ok(Pattern {
            len: len,
            pos: pos,
            matchers: m,
        })
    }

    fn parse(pattern: &str) -> Result<Vec<Matcher>, GlobError> {
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
                                return Err(GlobError::StrayBracket);
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
                                        return Err(GlobError::UnknownEscapeSequence);
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

                                _ => return Err(GlobError::UnknownEscapeSequence),
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

    pub fn matches(&self, string: &str) -> Option<usize> {
        match (self.matchers.len(), string.len()) {
            (n, 0) => {
                if n == 1 && self.matchers[0].is_many() {
                    return Some(0);
                } else {
                    return None;
                }
            }
            (0, _) => return None,
            (1, _) => {
                if self.matchers[0] == Matcher::Nop {
                    return None;
                }
            }
            _ => (),
        };

        debug_assert!(string.len() != 0);

        let chars: Vec<char> = match self.pos {
            MatchPosition::Prefix => string.chars().collect(),
            MatchPosition::Suffix => string.chars().rev().collect(),
        };

        let num_many = self.matchers.iter().filter(|&m| m == &Matcher::ManyChar).count();

        search_matches(&self.matchers, num_many, None, self.len, &chars)
    }
}

fn search_matches(matchers: &[Matcher],
                  num_many: usize,
                  span: Option<usize>,
                  matchlen: MatchLength,
                  input: &[char])
                  -> Option<usize> {
    let (matched, many_matched, len_matched) = matches_with_span(matchers, span, input);

    if matched == matchers.len() {
        debug_assert!(num_many == if many_matched { 1 } else { 0 });
        Some(len_matched)
    } else if !(num_many > 0 && span.is_none()) && (matched == 0 || num_many == 0) {
        None
    } else {
        match matchlen {
            MatchLength::Longest => {
                (0..input.len() + 1)
                    .rev()
                    .filter_map(|i| {
                        search_matches(&matchers[matched..],
                                       num_many - if many_matched { 1 } else { 0 },
                                       Some(i),
                                       matchlen,
                                       &input[len_matched..])
                    })
                    .map(|len| len + len_matched)
                    .next()
            }

            MatchLength::Shortest => {
                (0..input.len() + 1)
                    .filter_map(|i| {
                        search_matches(&matchers[matched..],
                                       num_many - if many_matched { 1 } else { 0 },
                                       Some(i),
                                       matchlen,
                                       &input[len_matched..])
                    })
                    .map(|len| len + len_matched)
                    .next()
            }
        }
    }
}

fn matches_with_span(matchers: &[Matcher],
                     span: Option<usize>,
                     input: &[char])
                     -> (usize, bool, usize) {
    let mut len_matched = 0;

    let mut input = &input[..];

    let mut ms = matchers.iter().enumerate();

    while !input.is_empty() {
        match ms.next() {
            None => return (matchers.len(), false, len_matched),

            Some((i, m)) => {
                match *m {
                    Matcher::Nop => (),

                    Matcher::Char(c) => {
                        if !input.is_empty() && input[0] == c {
                            len_matched += 1;
                            input = &input[1..];
                        } else {
                            return (i, false, len_matched);
                        }
                    }

                    Matcher::Str(ref sm) => {
                        if input.starts_with(sm) {
                            len_matched += sm.len();
                            input = &input[sm.len()..];
                        } else {
                            return (i, false, len_matched);
                        }
                    }

                    Matcher::AnyChar => {
                        if !input.is_empty() {
                            len_matched += 1;
                            input = &input[1..];
                        } else {
                            return (i, false, len_matched);
                        }
                    }

                    Matcher::Group(ref gm) => {
                        if !input.is_empty() && gm.matches(input[0]) {
                            len_matched += 1;
                            input = &input[1..];
                        } else {
                            return (i, false, len_matched);
                        }
                    }

                    Matcher::ManyChar => {
                        if let Some(s) = span {
                            if input.len() >= s {
                                len_matched += s;
                                return (i + 1, true, len_matched);
                            }
                        }
                        return (i, false, len_matched);
                    }
                }
            }
        }
    }

    (matchers.len() - ms.count(), false, len_matched)
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


// https://github.com/rust-lang-nursery/regex/blob/master/regex-syntax/src/parser.rs

type Class = &'static [(char, char)];
type NamedClasses = &'static [(&'static str, Class)];

// Classes must be in alphabetical order so that bsearch works.
// [:alnum:]      alphanumeric (== [0-9A-Za-z])
// [:alpha:]      alphabetic (== [A-Za-z])
// [:ascii:]      ASCII (== [\x00-\x7F])
// [:blank:]      blank (== [\t ])
// [:cntrl:]      control (== [\x00-\x1F\x7F])
// [:digit:]      digits (== [0-9])
// [:graph:]      graphical (== [!-~])
// [:lower:]      lower case (== [a-z])
// [:print:]      printable (== [ -~] == [ [:graph:]])
// [:punct:]      punctuation (== [!-/:-@[-`{-~])
// [:space:]      whitespace (== [\t\n\v\f\r ])
// [:upper:]      upper case (== [A-Z])
// [:word:]       word characters (== [0-9A-Za-z_])
// [:xdigit:]     hex digit (== [0-9A-Fa-f])
// Taken from: http://golang.org/pkg/regex/syntax/
const ASCII_CLASSES: NamedClasses = &[("alnum", &ALNUM),
                                      ("alpha", &ALPHA),
                                      ("ascii", &ASCII),
                                      ("blank", &BLANK),
                                      ("cntrl", &CNTRL),
                                      ("digit", &DIGIT),
                                      ("graph", &GRAPH),
                                      ("lower", &LOWER),
                                      ("print", &PRINT),
                                      ("punct", &PUNCT),
                                      ("space", &SPACE),
                                      ("upper", &UPPER),
                                      ("word", &WORD),
                                      ("xdigit", &XDIGIT)];

const ALNUM: Class = &[('0', '9'), ('A', 'Z'), ('a', 'z')];
const ALPHA: Class = &[('A', 'Z'), ('a', 'z')];
const ASCII: Class = &[('\x00', '\x7F')];
const BLANK: Class = &[(' ', ' '), ('\t', '\t')];
const CNTRL: Class = &[('\x00', '\x1F'), ('\x7F', '\x7F')];
const DIGIT: Class = &[('0', '9')];
const GRAPH: Class = &[('!', '~')];
const LOWER: Class = &[('a', 'z')];
const PRINT: Class = &[(' ', '~')];
const PUNCT: Class = &[('!', '/'), (':', '@'), ('[', '`'), ('{', '~')];
const SPACE: Class =
    &[('\t', '\t'), ('\n', '\n'), ('\x0B', '\x0B'), ('\x0C', '\x0C'), ('\r', '\r'), (' ', ' ')];
const UPPER: Class = &[('A', 'Z')];
const WORD: Class = &[('0', '9'), ('A', 'Z'), ('_', '_'), ('a', 'z')];
const XDIGIT: Class = &[('0', '9'), ('A', 'F'), ('a', 'f')];

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

        if inner.len() >= 3 && (inner[0] == ':' || (inner[0] == '!' && inner[1] == ':')) &&
           *inner.last().unwrap() == ':' {
            let inv = inner[0] == '!';
            let class = {
                let mut s = String::with_capacity(inner.len() - 2);
                for &c in &inner[1..inner.len() - 1] {
                    s.push(c);
                }
                s
            };

            if let Ok(i) = ASCII_CLASSES.binary_search_by(|&(c, _)| c.cmp(&class)) {
                let class = ASCII_CLASSES[i].1;
                for &(a, b) in class {
                    ts.push(GroupMatcherTarget::CharBetween(a, b));
                }
            }

            return GroupMatcher {
                target: ts, // ts can be empty
                invert: inv,
            };
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

    fn matches(&self, input: char) -> bool {
        for v in &self.target {
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

#[derive(Debug, Clone, PartialEq, Eq)]
enum Matcher {
    Nop,
    Str(Vec<char>),
    Char(char),
    Group(GroupMatcher),
    AnyChar,
    ManyChar,
}

impl Matcher {
    #[inline]
    fn is_many(&self) -> bool {
        match *self {
            Matcher::ManyChar => true,
            _ => false,
        }
    }

    fn reverse(self) -> Matcher {
        match self {
            Matcher::Str(mut s) => {
                s.reverse();
                Matcher::Str(s)
            }
            _ => self,
        }
    }

    fn combine_seq(self, other: Matcher) -> (Matcher, Option<Matcher>) {
        match (self, other) {
            (Matcher::Nop, other) => (other, None),

            (Matcher::Str(s1), Matcher::Str(mut s2)) => {
                let mut s1 = s1;
                s1.append(&mut s2);
                (Matcher::Str(s1), None)
            }

            (Matcher::Str(s), Matcher::Char(c)) => {
                let mut s = s;
                s.push(c);
                (Matcher::Str(s), None)
            }

            (Matcher::Char(c1), Matcher::Char(c2)) => {
                let mut s = Vec::with_capacity(2);
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
mod tests {
    use super::*;

    macro_rules! assert_match_eq {
        ($glob:expr, $len:expr, $pos:expr, $s:expr, $ret:expr) => {
            assert_eq!(Pattern::new($glob, $len, $pos).unwrap().matches($s), $ret);
        };
    }

    #[test]
    fn test_pattern() {
        assert_eq!(Pattern::new("a*b?c[-f\\]eda-d]",
                                MatchLength::Shortest,
                                MatchPosition::Suffix)
                       .unwrap(),
                   Pattern::new("a**b?c[defa-d\\]-]",
                                MatchLength::Shortest,
                                MatchPosition::Suffix)
                       .unwrap());

        let p = "";
        assert_match_eq!(p, MatchLength::Shortest, MatchPosition::Prefix, "abc", None);
        assert_match_eq!(p, MatchLength::Shortest, MatchPosition::Prefix, "", None);
        let p = "a";
        assert_match_eq!(p, MatchLength::Shortest, MatchPosition::Prefix, "", None);
        let p = "*";
        assert_match_eq!(p, MatchLength::Shortest, MatchPosition::Prefix, "", Some(0));

        let p = "a*b?d[feda-d]";
        assert_match_eq!(p,
                         MatchLength::Shortest,
                         MatchPosition::Prefix,
                         "abcde",
                         Some(5));
        assert_match_eq!(p,
                         MatchLength::Longest,
                         MatchPosition::Prefix,
                         "abcde",
                         Some(5));

        let p = "a*b";
        assert_match_eq!(p,
                         MatchLength::Shortest,
                         MatchPosition::Prefix,
                         "aababc",
                         Some(3));
        assert_match_eq!(p,
                         MatchLength::Shortest,
                         MatchPosition::Suffix,
                         "aababc",
                         None);
        assert_match_eq!(p,
                         MatchLength::Shortest,
                         MatchPosition::Suffix,
                         "aabab",
                         Some(2));
        assert_match_eq!(p,
                         MatchLength::Longest,
                         MatchPosition::Prefix,
                         "aababc",
                         Some(5));
        assert_match_eq!(p,
                         MatchLength::Longest,
                         MatchPosition::Suffix,
                         "aababc",
                         None);
        assert_match_eq!(p,
                         MatchLength::Longest,
                         MatchPosition::Suffix,
                         "aabab",
                         Some(5));

        let p = "a*b*c";
        assert_match_eq!(p,
                         MatchLength::Shortest,
                         MatchPosition::Prefix,
                         "abbcabc",
                         Some(4));
        assert_match_eq!(p,
                         MatchLength::Longest,
                         MatchPosition::Prefix,
                         "abbcabc",
                         Some(7));
        assert_match_eq!(p,
                         MatchLength::Longest,
                         MatchPosition::Prefix,
                         "abcabcabcabcabcabc_",
                         Some(18));
        assert_match_eq!(p,
                         MatchLength::Longest,
                         MatchPosition::Suffix,
                         "_abcabcabcabcabcabc",
                         Some(18));

        assert_match_eq!("a*b*c*d",
                         MatchLength::Shortest,
                         MatchPosition::Prefix,
                         "abced",
                         Some(5));
    }

    #[test]
    fn test_charclass() {
        assert_match_eq!("[:digit:]*[:lower:]",
                         MatchLength::Longest,
                         MatchPosition::Prefix,
                         "128Foo",
                         Some(6));

        assert_match_eq!("[:digit:]*[:lower:]",
                         MatchLength::Shortest,
                         MatchPosition::Prefix,
                         "128Foo",
                         Some(5));
    }
}
