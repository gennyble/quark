use std::{
	borrow::{Borrow, BorrowMut, Cow},
	iter::{self, Peekable},
	str::Chars,
};

struct Lexer {
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

	fn lex(&mut self, raw: String) {
		let lines = raw.lines();

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

		// Now the inline

		self.parse_for_inlines(&mut chars)
	}

	fn parse_for_inlines(&mut self, chars: &mut Peekable<Chars>) {
		let mut code = false;
		let mut current = String::new();
		let mut push_and_clear = |toks: &mut Vec<LexTok>, curr: &mut String| {
			if !curr.is_empty() {
				toks.push(LexTok::Text(curr.clone()));
				curr.clear();
			}
		};

		for ch in chars {
			if ch == '`' && !code {
				push_and_clear(&mut self.tokens, &mut current);
				self.tokens.push(LexTok::CodeStart);
				code = true;
				continue;
			} else if ch == '`' && code {
				push_and_clear(&mut self.tokens, &mut current);
				self.tokens.push(LexTok::CodeEnd);
				code = false;
				continue;
			} else if code {
				current.push(ch);
				continue;
			}

			current.push(ch);
		}

		// Be sure to push anything leftover
		push_and_clear(&mut self.tokens, &mut current);
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
	BlankLine,
}

impl LexTok {
	fn string(&self) -> Cow<str> {
		match self {
			LexTok::Header { level } => iter::repeat('#').take(*level as usize).collect(),
			LexTok::Text(txt) => Cow::Borrowed(&txt),
			LexTok::CodeBlockStart { lang } => Cow::Owned(format!("```{}", lang)),
			LexTok::CodeBlockEnd => Cow::Borrowed("```"),
			LexTok::EndOfLine => Cow::Borrowed("\n"),
			LexTok::CodeStart => Cow::Borrowed("`"),
			LexTok::CodeEnd => Cow::Borrowed("`"),
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
			LexTok::BlankLine => false,
		}
	}
}

struct Parser {
	last_line_blank: bool,
	tokens: Vec<Token>,
}

impl Parser {
	pub fn new() -> Self {
		Parser {
			last_line_blank: false,
			tokens: vec![],
		}
	}

	pub fn parse(&mut self, lex: Lexer) {
		let Lexer { tokens, .. } = lex;

		let mut iter = tokens.into_iter().peekable();

		loop {
			match iter.peek() {
				Some(tok) if tok.is_inline() => {
					println!("Isinline");
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
					iter.next(); // Consume the next token which is an EOL
				}
				None => return,
				//TODO: gen- we should emit and error on these
				Some(tok) => panic!("Should have been caught earlier {:?}", tok),
				Some(LexTok::EndOfLine) => (),
				Some(LexTok::CodeBlockEnd) => panic!("Saw CodeEnd"),
			}
		}
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

		match self.tokens.last_mut() {
			Some(Token::Paragraph { inner }) => {
				if !self.last_line_blank {
					inner.push(Inline::Break);
					inner.extend(built);
				} else {
					self.tokens.push(Token::Paragraph { inner: built })
				}
			}
			_ => self.tokens.push(Token::Paragraph { inner: built }),
		}

		self.last_line_blank = false;
	}

	fn inline_code(tokens: &mut Vec<Inline>, iter: &mut impl Iterator<Item = LexTok>) {
		match iter.next() {
			Some(LexTok::Text(code)) => {
				match iter.next() {
					Some(LexTok::CodeEnd) => (),
					//FIXME: gen- Error
					_ => panic!("Expected LexTok::CodeEnd"),
				}

				tokens.push(Inline::Code(code));
			}
			Some(LexTok::CodeEnd) => {
				tokens.push(Inline::Code(String::new()));
			}
			_ => panic!(),
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
enum Inline {
	Break,
	Text(String),
	Code(String),
}

impl Inline {
	pub fn text<S: Into<String>>(text: S) -> Self {
		Inline::Text(text.into())
	}

	pub fn code<S: Into<String>>(text: S) -> Self {
		Inline::Code(text.into())
	}
}

#[derive(Clone, Debug, PartialEq)]
enum Token {
	Header { level: u8, inner: Vec<Inline> },
	Paragraph { inner: Vec<Inline> },
	CodeBlock { lang: String, code: String },
}

#[cfg(test)]
mod test {
	use crate::{Inline, Lexer, Parser, Token};

	#[test]
	fn everything() {
		let str = "# Header\nSome text below the header.\nA break and some `code`\n\nNew paragraph!\n```lang\nCode!\nCode.\n```";

		let mut lex = Lexer::new();
		lex.lex(str.into());
		let mut parser = Parser::new();
		parser.parse(lex);

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
						Inline::code("code")
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

		let mut lex = Lexer::new();
		lex.lex(str.into());
		let mut parser = Parser::new();
		parser.parse(lex);

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

		let mut lex = Lexer::new();
		lex.lex(lvl1.into());
		let mut parser = Parser::new();
		parser.parse(lex);

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

		let mut lex = Lexer::new();
		lex.lex(txt.into());
		let mut parser = Parser::new();
		parser.parse(lex);

		assert_eq!(
			parser.tokens,
			vec![Token::CodeBlock {
				lang: String::from("html"),
				code: String::from("<html>\n\n<body>Body!</body>\n\n</html>\n")
			}]
		)
	}

	#[test]
	fn inline_code_parses_correctly() {
		let txt = "`code`";

		let mut lex = Lexer::new();
		lex.lex(txt.into());
		let mut parser = Parser::new();
		parser.parse(lex);

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

		let mut lex = Lexer::new();
		lex.lex(txt.into());
		let mut parser = Parser::new();
		parser.parse(lex);

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
}

/*
How in the damn do we parse things. We don't know how. Gaaaaaahhhhhhhd. Okay
okaaaaay okay okay, How does Quark work?

- Line breaks are respected. A line break will break the line, as it should, but
  not give the separation between paragraphs. For that you ned to use two line
  breaks. Quark is meant to parse and imitate the original file.
*/
