use core::fmt;
use std::{
	borrow::Cow,
	collections::HashMap,
	iter::{self, Peekable},
	str::Chars,
};

pub struct Lexer {
	in_code_block: bool,
	tokens: Vec<LexTok>,
}

impl Lexer {
	pub fn new() -> Self {
		Self {
			in_code_block: false,
			tokens: vec![],
		}
	}

	fn lex<S: AsRef<str>>(&mut self, raw: S) {
		let lines = raw.as_ref().lines();

		for line in lines {
			self.parse_line(line);
			self.tokens.push(LexTok::EndOfLine);
		}
	}

	fn parse_line(&mut self, line: &str) {
		// gen- Special code block handling because we don't want to parse anything in here
		if self.in_code_block {
			if line == "```" {
				self.in_code_block = false;
				self.tokens.push(LexTok::CodeBlockEnd);
			} else {
				self.tokens.push(LexTok::Text(line.into()));
			}

			return;
		} else if line.starts_with("```") {
			self.in_code_block = true;

			let lang = &line[3..];
			self.tokens
				.push(LexTok::CodeBlockStart { lang: lang.into() });

			return;
		}

		// Find blank lines and return early
		if line.is_empty() {
			self.tokens.push(LexTok::BlankLine);
			return;
		}

		let mut chars = line.chars().peekable();

		// Parse the block eleemnts

		self.header(&mut chars);

		// Loose parsing of reference links
		if line.starts_with('[') {
			match line.find("]:") {
				None => (),
				Some(idx) => {
					let name = line[1..idx].to_owned();
					let link = line[idx + 2..].to_owned();

					self.tokens.push(LexTok::LinkReference { name, link });
					return;
				}
			}
		}

		// Now the inline

		self.parse_for_inlines(&mut chars)
	}

	fn parse_for_inlines(&mut self, chars: &mut Peekable<Chars>) {
		let mut escape = false;
		let mut code = false;
		let mut reflink = false;
		let mut link = false;
		let mut current = String::new();

		macro_rules! push_and_clear {
			() => {
				if escape {
					current.push('\\');
				}

				if !current.is_empty() {
					self.tokens.push(LexTok::Text(current.clone()));
					current.clear();
				}
			};
		}

		for ch in chars {
			if ch == '\\' && !escape {
				escape = true;
				continue;
			} else if escape {
				escape = false;
				current.push(ch);
				continue;
			}

			if ch == '`' && !code && !reflink && !link {
				push_and_clear!();
				self.tokens.push(LexTok::CodeStart);
				code = true;
				continue;
			} else if ch == '`' && code && !reflink && !link {
				push_and_clear!();
				self.tokens.push(LexTok::CodeEnd);
				code = false;
				continue;
			}

			if ch == '{' && !reflink && !code && !link {
				push_and_clear!();
				self.tokens.push(LexTok::InterlinkStart);
				reflink = true;
				continue;
			} else if ch == '}' && reflink && !code && !link {
				push_and_clear!();
				self.tokens.push(LexTok::InterlinkEnd);
				reflink = false;
				continue;
			}

			if ch == '[' && !link && !code && !reflink {
				push_and_clear!();
				self.tokens.push(LexTok::LinkStart);
				link = true;
				continue;
			} else if ch == ']' && link && !code && !reflink {
				push_and_clear!();
				self.tokens.push(LexTok::LinkEnd);
				link = false;
				continue;
			}

			current.push(ch);
		}

		// Be sure to push anything leftover
		push_and_clear!();
	}

	fn header(&mut self, chars: &mut Peekable<Chars>) {
		let mut level = 0;
		loop {
			if let Some('#') = chars.peek() {
				// Throw away the hash
				let _ = chars.next();

				level += 1;

				if level == 5 {
					break;
				}
			//TODO: gen - fix this it's gross
			} else if chars.peek().is_some() && chars.peek().unwrap().is_whitespace() {
				break;
			//let _ = chars.next();
			} else {
				break;
			}
		}

		if level != 0 {
			self.tokens.push(LexTok::Header { level });
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
enum LexTok {
	Header { level: u8 },
	Text(String),
	CodeBlockStart { lang: String },
	CodeBlockEnd,
	EndOfLine,
	CodeStart,
	CodeEnd,
	InterlinkStart,
	InterlinkEnd,
	LinkReference { name: String, link: String },
	LinkStart,
	LinkEnd,
	BlankLine,
}

impl LexTok {
	fn string(&self) -> Cow<str> {
		match self {
			LexTok::Header { level } => iter::repeat('#').take(*level as usize).collect(),
			LexTok::Text(txt) => Cow::Borrowed(&txt),
			LexTok::CodeBlockStart { lang } => Cow::Owned(format!("```{lang}")),
			LexTok::CodeBlockEnd => Cow::Borrowed("```"),
			LexTok::EndOfLine => Cow::Borrowed("\n"),
			LexTok::CodeStart => Cow::Borrowed("`"),
			LexTok::CodeEnd => Cow::Borrowed("`"),
			LexTok::InterlinkStart => Cow::Borrowed("{"),
			LexTok::InterlinkEnd => Cow::Borrowed("}"),
			LexTok::LinkReference { name, link } => Cow::Owned(format!("[{name}]:{link}")),
			LexTok::LinkStart => Cow::Borrowed("["),
			LexTok::LinkEnd => Cow::Borrowed("]"),
			LexTok::BlankLine => Cow::Borrowed(""),
		}
	}

	/// Whether or not this LexTok starts a paragraph
	fn is_inline(&self) -> bool {
		match self {
			LexTok::Header { .. } => false,
			LexTok::Text(_) => true,
			LexTok::CodeBlockStart { .. } => false,
			LexTok::CodeBlockEnd => false,
			LexTok::EndOfLine => false,
			LexTok::CodeStart => true,
			LexTok::CodeEnd => true,
			LexTok::InterlinkStart => true,
			LexTok::InterlinkEnd => true,
			LexTok::LinkReference { .. } => false,
			LexTok::LinkStart => true,
			LexTok::LinkEnd => true,
			LexTok::BlankLine => false,
		}
	}
}

impl fmt::Display for LexTok {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.string())
	}
}

pub struct Parser {
	last_line_blank: bool,
	tokens: Vec<Token>,
	pub references: HashMap<String, String>,
}

impl Parser {
	pub fn new() -> Self {
		Parser {
			last_line_blank: false,
			tokens: vec![],
			references: HashMap::new(),
		}
	}

	pub fn parse<S: AsRef<str>>(&mut self, str: S) {
		let mut lex = Lexer::new();
		lex.lex(str.as_ref());

		macro_rules! discard_eol {
			($iter:expr) => {
				match $iter.next() {
					Some(LexTok::EndOfLine) => (),
					Some(tok) => panic!("tried to discord end of line but got {tok:?}"),
					_ => panic!("Tried to discard end of line but got None"),
				}
			};
		}

		let mut iter = lex.tokens.into_iter().peekable();

		loop {
			match iter.peek() {
				Some(tok) if tok.is_inline() => {
					self.paragraph(&mut iter);
					continue;
				}
				_ => (),
			}

			match iter.next() {
				Some(LexTok::Header { level }) => self.header(level, &mut iter),
				Some(LexTok::CodeBlockStart { lang }) => self.code_block(lang, &mut iter),
				Some(LexTok::BlankLine) => {
					self.last_line_blank = true;
					discard_eol!(iter)
				}
				Some(LexTok::LinkReference { name, link }) => {
					self.references.insert(name.trim().to_owned(), link);
					discard_eol!(iter)
				}
				None => return,
				//TODO: gen- we should emit and error on these
				Some(tok) => panic!("Should have been caught earlier {:?}", tok),
			}
		}
	}

	pub fn tokens<'a>(&'a self) -> std::slice::Iter<'a, Token> {
		self.tokens.iter()
	}

	fn build_line(lextokens: Vec<LexTok>) -> Vec<Inline> {
		let mut ret = vec![];
		let mut iter = lextokens.into_iter();

		//FIXME; gen- We need to make an inline parsing function that returns
		// its own vec
		loop {
			match iter.next() {
				None => break,
				Some(LexTok::CodeStart) => Self::inline_code(&mut ret, &mut iter),
				Some(LexTok::InterlinkStart) => Self::interlink(&mut ret, &mut iter),
				Some(LexTok::LinkStart) => Self::link(&mut ret, &mut iter),
				Some(LexTok::Text(txt)) => ret.push(Inline::Text(txt)),
				_ => (),
			}
		}

		ret
	}

	fn header(&mut self, level: u8, iter: &mut Peekable<impl Iterator<Item = LexTok>>) {
		let lextokens = Self::take_until(iter, LexTok::EndOfLine, false);
		let inner = Self::build_line(lextokens);

		self.tokens.push(Token::Header { level, inner })
	}

	fn code_block(&mut self, lang: String, iter: &mut Peekable<impl Iterator<Item = LexTok>>) {
		let mut lextokens = Self::take_until(iter, LexTok::CodeBlockEnd, false).into_iter();

		match lextokens.next() {
			Some(LexTok::EndOfLine) => (),
			//FIXME: gen- this should be an error
			_ => panic!("no end of line"),
		}

		let mut code_string = String::new();
		for tok in lextokens {
			match tok {
				LexTok::Text(txt) => code_string.push_str(&txt),
				LexTok::EndOfLine => code_string.push('\n'),
				//FIXME: gen- error
				_ => panic!("Should not be here :pensive:"),
			}
		}

		match iter.next() {
			Some(LexTok::EndOfLine) => (),
			_ => panic!(),
		}

		self.tokens.push(Token::CodeBlock {
			lang,
			code: code_string,
		})
	}

	fn paragraph(&mut self, iter: &mut Peekable<impl Iterator<Item = LexTok>>) {
		let lextokens = Self::take_until(iter, LexTok::EndOfLine, false);
		let built = Self::build_line(lextokens);

		// Make sure the last token is a paragraph and add a break if we need one
		match self.tokens.last_mut() {
			Some(Token::Paragraph { inner }) => {
				if !self.last_line_blank {
					inner.push(Inline::Break);
				} else {
					self.tokens.push(Token::Paragraph { inner: vec![] });
				}
			}
			_ => self.tokens.push(Token::Paragraph { inner: vec![] }),
		}

		// Keep pushing inlines, collapsing Text if we ended up with more than one
		for inl in built {
			match self.tokens.last_mut().unwrap() {
				Token::Paragraph { inner } => {
					if let Inline::Text(ref str) = inl {
						if let Some(Inline::Text(ref mut text_str)) = inner.last_mut() {
							text_str.push_str(str);
							continue;
						}
					}

					inner.push(inl);
				}
				_ => unreachable!(),
			}
		}

		self.last_line_blank = false;
	}

	fn inline_code(tokens: &mut Vec<Inline>, iter: &mut impl Iterator<Item = LexTok>) {
		match iter.next() {
			Some(LexTok::Text(code)) => {
				match iter.next() {
					Some(LexTok::CodeEnd) => (),
					//FIXME: gen- Error
					Some(tok) => panic!("Expected LexTok::CodeEnd but got {tok:?}"),
					None => panic!("Expected LexTok::CodeEnd but got None"),
				}

				tokens.push(Inline::Code(code));
			}
			Some(LexTok::CodeEnd) => {
				tokens.push(Inline::Code(String::new()));
			}
			None => tokens.push(Inline::Text(LexTok::CodeStart.string().into_owned())),
			_ => panic!(),
		}
	}

	fn interlink(tokens: &mut Vec<Inline>, iter: &mut impl Iterator<Item = LexTok>) {
		match iter.next() {
			Some(LexTok::Text(reftext)) => {
				match iter.next() {
					Some(LexTok::InterlinkEnd) => (),
					//FIXME: gen- Error
					_ => panic!("Expected LexTok::InterlinkEnd"),
				}

				tokens.push(Inline::Interlink(Self::make_link(reftext)));
			}
			Some(LexTok::InterlinkEnd) => tokens.push(Inline::Text(format!(
				"{}{}",
				LexTok::InterlinkStart,
				LexTok::InterlinkEnd
			))),
			None => tokens.push(Inline::Text(LexTok::InterlinkStart.to_string())),
			_ => panic!(),
		}
	}

	fn link(tokens: &mut Vec<Inline>, iter: &mut impl Iterator<Item = LexTok>) {
		match iter.next() {
			Some(LexTok::Text(link)) => {
				match iter.next() {
					Some(LexTok::LinkEnd) => (),
					//FIXME: gen- Error
					_ => panic!("Expected LexTok::LinkEnd"),
				}

				match link.strip_prefix('!') {
					None => tokens.push(Inline::Link(Self::make_link(link))),
					Some(stripped) => {
						tokens.push(Inline::ReferenceLink(Self::make_link(stripped.to_owned())))
					}
				}
			}
			Some(LexTok::LinkEnd) => tokens.push(Inline::Text(format!(
				"{}{}",
				LexTok::LinkStart,
				LexTok::LinkEnd
			))),
			None => tokens.push(Inline::Text(LexTok::LinkStart.to_string())),
			_ => panic!(),
		}
	}

	fn make_link<S: AsRef<str>>(s: S) -> Link {
		let s = s.as_ref();

		match s.split_once('|') {
			None => Link::nameless(s),
			Some((name, link)) => Link::new(name, link),
		}
	}

	fn take_until<I, T>(iter: &mut I, end: T, include_end: bool) -> Vec<T>
	where
		I: Iterator<Item = T>,
		T: PartialEq,
	{
		let mut ret = vec![];

		loop {
			match iter.next() {
				Some(item) if item == end => {
					if include_end {
						ret.push(end);
					}

					break ret;
				}
				Some(item) => ret.push(item),
				None => break ret,
			}
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub struct Link {
	pub name: Option<String>,
	pub location: String,
}

impl Link {
	pub fn nameless<S: Into<String>>(location: S) -> Link {
		Link {
			name: None,
			location: location.into(),
		}
	}

	pub fn new<S: Into<String>, N: Into<String>>(name: N, location: S) -> Link {
		Link {
			name: Some(name.into()),
			location: location.into(),
		}
	}
}

impl fmt::Display for Link {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match &self.name {
			None => write!(f, "{}", self.location),
			Some(name) => write!(f, "{name}|{}", self.location),
		}
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Inline {
	Break,
	Text(String),
	Code(String),
	Interlink(Link),
	Link(Link),
	ReferenceLink(Link),
}

impl Inline {
	pub fn text<S: Into<String>>(text: S) -> Self {
		Inline::Text(text.into())
	}

	pub fn code<S: Into<String>>(text: S) -> Self {
		Inline::Code(text.into())
	}

	pub fn interlink(link: Link) -> Self {
		Inline::Interlink(link)
	}

	pub fn link(link: Link) -> Self {
		Inline::Link(link)
	}

	pub fn reference_link(link: Link) -> Self {
		Inline::ReferenceLink(link)
	}
}

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
	Header { level: u8, inner: Vec<Inline> },
	Paragraph { inner: Vec<Inline> },
	CodeBlock { lang: String, code: String },
}

#[cfg(test)]
mod test {
	use std::collections::HashMap;

	use crate::{Inline, Link, Parser, Token};

	#[test]
	fn everything() {
		let str = r###"
# Header
Some text below the header.
A break and some `code` and a {interlink}

Okay, linkfest! [link] and [!link]
Now some named stuff: {Named Interlink|interlink}[Named Link|link][!Named Reference|reflink]

[link]: ref!

New paragraph!
```lang
Code!
Code.
```"###;

		let mut parser = Parser::new();
		parser.parse(str);

		let mut rfs = HashMap::new();
		rfs.insert("link".to_string(), " ref!".to_string());

		assert_eq!(parser.references, rfs);

		assert_eq!(
			parser.tokens,
			vec![
				Token::Header {
					level: 1,
					inner: vec![Inline::text(" Header")]
				},
				Token::Paragraph {
					inner: vec![
						Inline::text("Some text below the header."),
						Inline::Break,
						Inline::text("A break and some "),
						Inline::code("code"),
						Inline::text(" and a "),
						Inline::interlink(Link::nameless("interlink"))
					]
				},
				Token::Paragraph {
					inner: vec![
						Inline::text("Okay, linkfest! "),
						Inline::link(Link::nameless("link")),
						Inline::text(" and "),
						Inline::reference_link(Link::nameless("link")),
						Inline::Break,
						Inline::text("Now some named stuff: "),
						Inline::interlink(Link::new("Named Interlink", "interlink")),
						Inline::link(Link::new("Named Link", "link")),
						Inline::reference_link(Link::new("Named Reference", "reflink"))
					]
				},
				Token::Paragraph {
					inner: vec![Inline::text("New paragraph!")]
				},
				Token::CodeBlock {
					lang: String::from("lang"),
					code: String::from("Code!\nCode.\n")
				}
			]
		)
	}

	#[test]
	fn breaks_paragraphs_correctly() {
		let str = "Text\nAnother Line\n\nAnother Paragraph";

		let mut parser = Parser::new();
		parser.parse(str);

		assert_eq!(
			parser.tokens,
			vec![
				Token::Paragraph {
					inner: vec![
						Inline::text("Text"),
						Inline::Break,
						Inline::text("Another Line")
					]
				},
				Token::Paragraph {
					inner: vec![Inline::text("Another Paragraph")]
				}
			]
		)
	}

	#[test]
	fn headers_parse_correctly() {
		let lvl1 = "# Header!\nAnd some text below";

		let mut parser = Parser::new();
		parser.parse(lvl1);

		assert_eq!(
			parser.tokens,
			vec![
				Token::Header {
					level: 1,
					inner: vec![Inline::text(" Header!")]
				},
				Token::Paragraph {
					inner: vec![Inline::text("And some text below")]
				}
			]
		)
	}

	#[test]
	fn code_blocks_parse_correctly() {
		let txt = "```html\n<html>\n\n<body>Body!</body>\n\n</html>\n```";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(
			parser.tokens,
			vec![Token::CodeBlock {
				lang: String::from("html"),
				code: String::from("<html>\n\n<body>Body!</body>\n\n</html>\n")
			}]
		)
	}

	#[test]
	fn escape_parses() {
		let txt = "`\\``";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(
			parser.tokens,
			vec![Token::Paragraph {
				inner: vec![Inline::Code(String::from("`"))]
			}]
		)
	}

	#[test]
	fn inline_code_parses_correctly() {
		let txt = "`code`";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(
			parser.tokens,
			vec![Token::Paragraph {
				inner: vec![Inline::Code(String::from("code"))]
			}]
		)
	}

	#[test]
	fn inline_code_parses_correctly_after_text() {
		let txt = "Normal text and then `code`";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(
			parser.tokens,
			vec![Token::Paragraph {
				inner: vec![
					Inline::Text(String::from("Normal text and then ")),
					Inline::Code(String::from("code"))
				]
			}]
		)
	}

	#[test]
	fn interlink_parses_correctly() {
		let txt = "{interlink}";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(
			parser.tokens,
			vec![Token::Paragraph {
				inner: vec![Inline::interlink(Link::nameless("interlink"))]
			}]
		)
	}

	#[test]
	fn empty_reflink_is_text() {
		let txt = "{}";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(
			parser.tokens,
			vec![Token::Paragraph {
				inner: vec![Inline::text("{}")]
			}]
		)
	}

	#[test]
	fn parses_link_reference() {
		let txt = "[ref]: link!";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(parser.references.get("ref"), Some(&" link!".to_string()))
	}

	#[test]
	fn link_parses_correctly() {
		let txt = "[link]";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(
			parser.tokens,
			vec![Token::Paragraph {
				inner: vec![Inline::link(Link::nameless("link"))]
			}]
		)
	}

	#[test]
	fn reference_link_parses_correctly() {
		let txt = "[!link]";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(
			parser.tokens,
			vec![Token::Paragraph {
				inner: vec![Inline::reference_link(Link::nameless("link"))]
			}]
		)
	}

	#[test]
	fn named_links_parse_correctly() {
		let txt = "[Named | link]";

		let mut parser = Parser::new();
		parser.parse(txt);

		assert_eq!(
			parser.tokens,
			vec![Token::Paragraph {
				inner: vec![Inline::link(Link::new("Named ", " link"))]
			}]
		)
	}
}

/*
How in the damn do we parse things. We don't know how. Gaaaaaahhhhhhhd. Okay
okaaaaay okay okay, How does Quark work?

- Line breaks are respected. A line break will break the line, as it should, but
  not give the separation between paragraphs. For that you ned to use two line
  breaks. Quark is meant to parse and imitate the original file.
*/
