use std::collections::HashMap;
use std::iter::Peekable;

use anyhow::{Result, bail};
use thiserror::Error;

#[derive(Error, Debug)]
enum ParseError {
    #[error("parse: Bad tokens")]
    Tokens,
    #[error("parse: Malformed")]
    Malformed,
    #[error("parse: End of text")]
    Eof,
}

#[derive(Clone, Debug)]
enum Part {
    Atom(String),
    List(Vec<Part>),
    Form(Vec<Part>),
}

#[derive(Debug)]
struct Register {
    logical: Vec<Part>,
    physical: (Vec<Part>, Vec<Part>),
    dict: HashMap<String, Vec<String>>,
}

type It<'a> = Peekable<std::slice::Iter<'a, &'a str>>;

#[derive(Clone, Copy, Debug)]
pub enum Radix {
    Dec = 10,
    Hex = 16,
}

impl Register {
    pub fn parse(src: &str) -> Result<Register> {
        let pieces = src.split(';').collect::<Vec<_>>();
        if pieces.is_empty() {
            bail!("poorly formed");
        }
        let logical = parse(pieces.get(0).ok_or(ParseError::Malformed)?, Radix::Dec)?;
        let physical = parse_phys(pieces.get(1).map_or("", |v| v))?;
        let dict = parse_dict(pieces.get(2..pieces.len()).map_or(&[], |v| v))?;
        Ok(Register {
            logical,
            physical,
            dict,
        })
    }

    pub fn expand(&self) -> Vec<String> {
        expand_form(&self.logical)
    }

    pub fn expand_phys(&self) -> Vec<String> {
        let (names, values) = &self.physical;
        let names = expand_form(&names);
        let values = expand_form(&values);
        let max = usize::max(names.len(), values.len());
        names
            .into_iter()
            .cycle()
            .take(max)
            .zip(values.iter().cycle())
            .map(|(n, v)| n + "x" + v)
            .collect()
    }

    pub fn dict(&self) -> HashMap<String, Vec<String>> {
        self.dict.clone()
    }

    pub fn dump(&self) -> Result<()> {
        dump(self)
    }
}

fn parse(src: &str, radix: Radix) -> Result<Vec<Part>> {
    let tokens = tokenize(src);
    let mut iter = tokens.iter().peekable();
    match parse_list(&mut iter, radix)? {
        Part::Form(form) => Ok(form),
        p => Ok(vec![p]),
    }
}

fn parse_phys(str: &str) -> Result<(Vec<Part>, Vec<Part>)> {
    let pieces = str.split('x').collect::<Vec<_>>();
    if pieces.len() != 2 {
        bail!("bad physical");
    }
    let names = parse(&pieces[0], Radix::Dec)?;
    let vals = parse(&pieces[1], Radix::Hex)?;
    Ok((names, vals))
}

fn parse_dict(srcs: &[&str]) -> Result<HashMap<String, Vec<String>>> {
    let mut dict = HashMap::new();
    for src in srcs {
        let v = src.split('=').collect::<Vec<_>>();
        if v.len() != 2 {
            bail!(ParseError::Tokens);
        }
        let a = parse(v[0], Radix::Dec)?;
        let a = expand_form(&a);
        let b = parse(v[1], Radix::Hex)?;
        let b = expand_form(&b);
        if a.len() == b.len() {
            for (a, b) in a.into_iter().zip(b.into_iter()) {
                dict.insert(a, vec![b]);
            }
        } else {
            bail!("Unhandled case");
        }
    }
    Ok(dict)
}

const SEPS: &str = "[]=,:;";

// "Tokenize" the source string.
fn tokenize(mut src: &str) -> Vec<&str> {
    let mut tokens = vec![];
    while !src.is_empty() {
        src = src.trim();
        let pos = src
            .find(|c: char| SEPS.contains(c) || c.is_whitespace())
            .unwrap_or(src.len());
        let (tok, rest) = src.split_at(pos);
        if !tok.is_empty() {
            tokens.push(tok.trim());
        }
        src = rest.trim();
        for sep in SEPS.chars() {
            if src.starts_with(sep) {
                let (tok, rest) = src.split_at(sep.len_utf8());
                tokens.push(tok.trim());
                src = rest.trim();
            }
        }
    }
    tokens
}

fn parse_part(it: &mut It, radix: Radix) -> Result<Part> {
    let mut part = Vec::<Part>::new();
    while let Some(_) = it.peek() {
        match it.next() {
            Some(&"[") => part.push(parse_list(it, radix)?),
            Some(&atom) => part.push(parse_atom(atom, it, radix)?),
            None => bail!("error"),
        }
        match it.peek() {
            Some(&&"]" | &&",") | None => break,
            _ => {}
        }
    }
    Ok(if part.len() > 1 {
        Part::Form(part)
    } else {
        part[0].clone()
    })
}

fn parse_list(it: &mut It, radix: Radix) -> Result<Part> {
    let mut list = vec![];
    while let Some(_) = it.peek() {
        list.push(parse_part(it, radix)?);
        match it.peek() {
            Some(&&",") => {
                let _ = it.next();
            }
            Some(&&"]") => {
                let _ = it.next();
                break;
            }
            None => break,
            _ => bail!(ParseError::Tokens),
        }
    }
    Ok(Part::List(list))
}

fn parse_atom(base: &str, it: &mut It, radix: Radix) -> Result<Part> {
    let mut atom = String::from(base);
    if let Some(&&":") = it.peek() {
        let _ = it.next();
        let Some(&token) = it.next() else {
            bail!("format");
        };
        atom = atom + ":" + token;
    }
    let atoms = if let Ok(numbers) = parse_inst_num(&atom, radix) {
        Part::List(numbers.into_iter().map(|num| Part::Atom(num)).collect())
    } else {
        Part::Atom(base.into())
    };
    Ok(atoms)
}

fn parse_inst_num(src: &str, radix: Radix) -> Result<Vec<String>> {
    if src.is_empty() {
        bail!(ParseError::Eof);
    }
    if !src.contains(':') {
        return Ok(vec![src.into()]);
    }
    let mut split = src.split(':').fuse();
    let (end, start, none) = (split.next(), split.next(), split.next());
    let (astr, bstr) = match (start, end, none) {
        (Some(b), Some(a), None) => (a, b),
        _ => bail!(ParseError::Tokens),
    };
    let a = u32::from_str_radix(astr, radix as u32)?;
    let b = u32::from_str_radix(bstr, radix as u32)?;
    let r = u32::min(a, b)..=u32::max(a, b);
    let width = usize::min(astr.len(), bstr.len());
    let v: Vec<_> = if a > b {
        r.rev().collect()
    } else {
        r.collect()
    };
    Ok(v.into_iter()
        .map(|k| match radix {
            Radix::Hex => format!("{k:0>width$x}"),
            Radix::Dec => format!("{k:0>width$}"),
        })
        .collect())
}

fn expand_form(parts: &[Part]) -> Vec<String> {
    fn inner(prefaces: Vec<String>, parts: &[Part]) -> Vec<String> {
        if let Some((first, rest)) = parts.split_first() {
            let these = match first {
                Part::Atom(atom) => vec![atom.clone()],
                Part::Form(parts) => expand_form(&parts),
                Part::List(parts) => parts
                    .iter()
                    .map(|part| expand_form(std::slice::from_ref(part)))
                    .flatten()
                    .collect(),
            };
            let mut expanded = vec![];
            for prefix in prefaces.into_iter() {
                for this in these.iter() {
                    expanded.push(prefix.clone() + this);
                }
            }
            inner(expanded, rest)
        } else {
            prefaces
        }
    }
    if parts.is_empty() {
        return vec![];
    }
    inner(vec![String::new()], parts)
}

fn dump(reg: &Register) -> Result<()> {
    let logical = reg.expand();
    let physical = reg.expand_phys();
    let dict = reg.dict();
    let mut xphys = vec![];
    for (k, mut phys) in physical.into_iter().enumerate() {
        let toks = phys.split('x').collect::<Vec<_>>();
        if toks.len() != 2 {
            println!("phys: {phys}");
            bail!("bad tokens in phys");
        }
        let key = toks[0];
        let offset = toks[1];
        if let Some(values) = dict.get(key) {
            let value = values.iter().cycle().nth(k).unwrap();
            let vs = value
                .chars()
                .filter(char::is_ascii_hexdigit)
                .collect::<String>();
            let os = offset
                .chars()
                .filter(char::is_ascii_hexdigit)
                .collect::<String>();
            let v = u32::from_str_radix(&vs, 16)?;
            let o = u32::from_str_radix(&os, 16)?;
            let addr = v + o;
            phys = format!("{} + {} ==> {:#08x}", value, offset, addr);
        }
        xphys.push(phys);
    }
    assert_eq!(logical.len(), xphys.len());
    for (log, phys) in logical.iter().zip(xphys.iter()) {
        println!("{log} -> {phys}");
    }
    Ok(())
}

fn main() {
    let args = std::env::args();
    if args.len() < 2 {
        eprintln!("usage: amdregname reg[s]");
        std::process::exit(1);
    }
    for reg in std::env::args().skip(1) {
        let reg = Register::parse(&reg).expect("parse");
        reg.dump().expect("dump");
    }
}

// Tests follow.
#[cfg(test)]
mod parse_inst_num_tests {
    use super::*;

    #[test]
    fn single() {
        let v = parse_inst_num("4", Radix::Hex);
        assert!(v.is_ok_and(|v| {
            assert_eq!(&v, &["4"]);
            true
        }));
    }

    #[test]
    fn range() {
        let v = parse_inst_num("2:0", Radix::Dec);
        assert!(v.is_ok_and(|v| {
            assert_eq!(&v, &["2", "1", "0"]);
            true
        }));
        let v = parse_inst_num("0:2", Radix::Dec);
        assert!(v.is_ok_and(|v| {
            assert_eq!(&v, &["0", "1", "2"]);
            true
        }));
        let v = parse_inst_num("B:9", Radix::Hex);
        assert!(v.is_ok_and(|v| {
            assert_eq!(&v, &["b", "a", "9"]);
            true
        }));
        let v = parse_inst_num("0b:09", Radix::Hex);
        assert!(v.is_ok_and(|v| {
            assert_eq!(&v, &["0b", "0a", "09"]);
            true
        }));
    }

    #[test]
    fn empty() {
        assert!(parse_inst_num("", Radix::Hex).is_err());
    }

    #[test]
    fn multi() {
        assert!(parse_inst_num("1:2:3", Radix::Dec).is_err());
    }
}

#[cfg(test)]
mod expand_form_tests {
    use super::*;

    fn s(src: &str) -> String {
        String::from(src)
    }

    #[test]
    fn empty() {
        assert_eq!(expand_form(&[]), Vec::<String>::new())
    }

    #[test]
    fn single_atom() {
        let parts = vec![Part::Atom(s("aaa"))];
        assert_eq!(expand_form(&parts), vec!["aaa".to_owned()]);
    }

    #[test]
    fn single_list() {
        let parts = vec![Part::List(vec![Part::Atom(s("aaa"))])];
        assert_eq!(expand_form(&parts), vec![s("aaa")]);

        let parts = vec![Part::List(vec![Part::Atom(s("aaa")), Part::Atom(s("bbb"))])];
        assert_eq!(expand_form(&parts), vec![s("aaa"), s("bbb")]);
    }

    #[test]
    fn atom_list() {
        let parts = vec![
            Part::Atom(s("aaa")),
            Part::List(vec![Part::Atom(s("bbb")), Part::Atom(s("ccc"))]),
        ];
        assert_eq!(expand_form(&parts), vec![s("aaabbb"), s("aaaccc")]);
    }

    #[test]
    fn atom_list_atom() {
        let parts = vec![
            Part::Atom(s("aaa")),
            Part::List(vec![Part::Atom(s("bbb")), Part::Atom(s("ccc"))]),
            Part::Atom(s("1")),
        ];
        assert_eq!(expand_form(&parts), vec![s("aaabbb1"), s("aaaccc1")]);
    }

    #[test]
    fn atom_list_atom_list() {
        let parts = vec![
            Part::Atom(s("aaa")),
            Part::List(vec![Part::Atom(s("bbb")), Part::Atom(s("ccc"))]),
            Part::Atom(s("1")),
            Part::List(vec![Part::Atom(s("d")), Part::Atom(s("e"))]),
        ];
        assert_eq!(
            expand_form(&parts),
            vec![s("aaabbb1d"), s("aaabbb1e"), s("aaaccc1d"), s("aaaccc1e")]
        );
    }

    // Test a form that contains a nested form.  This comes from
    // a string such as,
    // `_inst[PCIE[5,3,2:0]]_nbio0_aliasSMN`
    #[test]
    fn nested() {
        let parts = vec![
            Part::Atom(s("_inst")),
            Part::Form(vec![
                Part::Atom(s("PCIE")),
                Part::List(vec![
                    Part::Atom(s("5")),
                    Part::Atom(s("3")),
                    Part::Atom(s("2")),
                    Part::Atom(s("1")),
                    Part::Atom(s("0")),
                ]),
            ]),
            Part::Atom(s("_nbio0_aliasSMN")),
        ];
        let expanded = expand_form(&parts);
        assert_eq!(
            expanded,
            vec![
                s("_instPCIE5_nbio0_aliasSMN"),
                s("_instPCIE3_nbio0_aliasSMN"),
                s("_instPCIE2_nbio0_aliasSMN"),
                s("_instPCIE1_nbio0_aliasSMN"),
                s("_instPCIE0_nbio0_aliasSMN"),
            ]
        );
    }
}
