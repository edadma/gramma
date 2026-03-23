package io.github.edadma.gramma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StdLexerTests extends AnyFreeSpec with Matchers:

  object TestLexer extends StdLexer:
    delimiters ++= List("<=", ">=", "==", "!=", "<", ">", "=", "+", "-", "*", "/", "(", ")", ",", ";", "{", "}", ".")
    reserved ++= List("if", "then", "else", "true", "false", "null", "in", "int")

  def lex(input: String): Either[ParseError, List[(StdTokenKind, String)]] =
    TestLexer.tokenize(input).map(_.toList.map(t => (t.kind, t.text)))

  def lexKinds(input: String): Either[ParseError, List[StdTokenKind]] =
    TestLexer.tokenize(input).map(_.toList.map(_.kind))

  // --- Identifiers ---

  "identifier" in {
    lex("foo") shouldBe Right(List((StdTokenKind.Ident, "foo")))
  }

  "identifier with digits" in {
    lex("x42") shouldBe Right(List((StdTokenKind.Ident, "x42")))
  }

  "identifier with underscores" in {
    lex("_foo_bar") shouldBe Right(List((StdTokenKind.Ident, "_foo_bar")))
  }

  "single letter identifier" in {
    lex("x") shouldBe Right(List((StdTokenKind.Ident, "x")))
  }

  "identifier starting with underscore and digit" in {
    lex("_1") shouldBe Right(List((StdTokenKind.Ident, "_1")))
  }

  // --- Keywords ---

  "keyword" in {
    lex("if") shouldBe Right(List((StdTokenKind.Keyword, "if")))
  }

  "keyword vs identifier — keyword prefix of identifier" in {
    // "int" is a keyword, "internal" starts with "int" but is an identifier
    lex("internal") shouldBe Right(List((StdTokenKind.Ident, "internal")))
  }

  "keyword vs identifier — identifier prefix of keyword" in {
    // "in" is a keyword
    lex("in") shouldBe Right(List((StdTokenKind.Keyword, "in")))
  }

  "keyword followed by delimiter" in {
    lex("if(") shouldBe Right(List(
      (StdTokenKind.Keyword, "if"),
      (StdTokenKind.Delimiter, "("),
    ))
  }

  "multiple keywords" in {
    lex("if true then false else null") shouldBe Right(List(
      (StdTokenKind.Keyword, "if"),
      (StdTokenKind.Keyword, "true"),
      (StdTokenKind.Keyword, "then"),
      (StdTokenKind.Keyword, "false"),
      (StdTokenKind.Keyword, "else"),
      (StdTokenKind.Keyword, "null"),
    ))
  }

  // --- Numeric literals ---

  "integer" in {
    lex("42") shouldBe Right(List((StdTokenKind.NumericLit, "42")))
  }

  "single digit" in {
    lex("0") shouldBe Right(List((StdTokenKind.NumericLit, "0")))
  }

  "decimal" in {
    lex("3.14") shouldBe Right(List((StdTokenKind.NumericLit, "3.14")))
  }

  "decimal with trailing zeros" in {
    lex("1.00") shouldBe Right(List((StdTokenKind.NumericLit, "1.00")))
  }

  "number followed by dot delimiter" in {
    // "42." — the dot is consumed as decimal point, then needs a digit
    lex("42.x") shouldBe a[Left[?, ?]]
  }

  // --- String literals ---

  "simple string" in {
    lex("\"hello\"") shouldBe Right(List((StdTokenKind.StringLit, "hello")))
  }

  "empty string" in {
    lex("\"\"") shouldBe Right(List((StdTokenKind.StringLit, "")))
  }

  "string with escapes" in {
    lex("\"a\\nb\"") shouldBe Right(List((StdTokenKind.StringLit, "a\nb")))
  }

  "string with escaped quote" in {
    lex("\"say \\\"hi\\\"\"") shouldBe Right(List((StdTokenKind.StringLit, "say \"hi\"")))
  }

  "unclosed string" in {
    lex("\"hello") shouldBe a[Left[?, ?]]
  }

  // --- Delimiters ---

  "single-char delimiter" in {
    lex("+") shouldBe Right(List((StdTokenKind.Delimiter, "+")))
  }

  "multi-char delimiter" in {
    lex("<=") shouldBe Right(List((StdTokenKind.Delimiter, "<=")))
  }

  "longest match — <= not < then =" in {
    lex("<=") shouldBe Right(List((StdTokenKind.Delimiter, "<=")))
    lex("< =") shouldBe Right(List(
      (StdTokenKind.Delimiter, "<"),
      (StdTokenKind.Delimiter, "="),
    ))
  }

  "all multi-char delimiters" in {
    lex(">= != ==") shouldBe Right(List(
      (StdTokenKind.Delimiter, ">="),
      (StdTokenKind.Delimiter, "!="),
      (StdTokenKind.Delimiter, "=="),
    ))
  }

  "delimiters without whitespace" in {
    lex("(x+y)") shouldBe Right(List(
      (StdTokenKind.Delimiter, "("),
      (StdTokenKind.Ident, "x"),
      (StdTokenKind.Delimiter, "+"),
      (StdTokenKind.Ident, "y"),
      (StdTokenKind.Delimiter, ")"),
    ))
  }

  // --- Whitespace ---

  "skips leading whitespace" in {
    lex("  x") shouldBe Right(List((StdTokenKind.Ident, "x")))
  }

  "skips trailing whitespace" in {
    lex("x  ") shouldBe Right(List((StdTokenKind.Ident, "x")))
  }

  "skips newlines and tabs" in {
    lex("x\n\ty") shouldBe Right(List(
      (StdTokenKind.Ident, "x"),
      (StdTokenKind.Ident, "y"),
    ))
  }

  "empty input" in {
    lex("") shouldBe Right(Nil)
  }

  "whitespace only" in {
    lex("   \n\t  ") shouldBe Right(Nil)
  }

  // --- Position tracking ---

  "positions are correct" in {
    val tokens = TestLexer.tokenize("x + 42").toOption.get
    tokens(0).pos shouldBe Pos(1, 1, "x + 42")
    tokens(1).pos shouldBe Pos(1, 3, "x + 42")
    tokens(2).pos shouldBe Pos(1, 5, "x + 42")
  }

  "multiline positions" in {
    val tokens = TestLexer.tokenize("x\ny").toOption.get
    tokens(0).pos shouldBe Pos(1, 1, "x")
    tokens(1).pos shouldBe Pos(2, 1, "y")
  }

  // --- Error cases ---

  "error on invalid character" in {
    lex("x @ y") shouldBe a[Left[?, ?]]
  }

  "error reports position" in {
    val result = TestLexer.tokenize("x @ y")
    result match
      case Left(err) =>
        err.pos.col shouldBe 3
        err.pos.line shouldBe 1
      case Right(_) => fail("expected error")
  }

  // --- Mixed token sequences ---

  "realistic expression" in {
    lex("if x >= 42 then y + 1 else z") shouldBe Right(List(
      (StdTokenKind.Keyword, "if"),
      (StdTokenKind.Ident, "x"),
      (StdTokenKind.Delimiter, ">="),
      (StdTokenKind.NumericLit, "42"),
      (StdTokenKind.Keyword, "then"),
      (StdTokenKind.Ident, "y"),
      (StdTokenKind.Delimiter, "+"),
      (StdTokenKind.NumericLit, "1"),
      (StdTokenKind.Keyword, "else"),
      (StdTokenKind.Ident, "z"),
    ))
  }

  "function call" in {
    lex("f(x, 3.14, \"hello\")") shouldBe Right(List(
      (StdTokenKind.Ident, "f"),
      (StdTokenKind.Delimiter, "("),
      (StdTokenKind.Ident, "x"),
      (StdTokenKind.Delimiter, ","),
      (StdTokenKind.NumericLit, "3.14"),
      (StdTokenKind.Delimiter, ","),
      (StdTokenKind.StringLit, "hello"),
      (StdTokenKind.Delimiter, ")"),
    ))
  }

  // --- Custom StdLexer: single-quoted strings ---

  "custom string quote" in {
    object SingleQuoteLexer extends StdLexer:
      override protected def stringQuote: Char = '\''

    val result = SingleQuoteLexer.tokenize("'hello'")
    result.map(_.toList.map(t => (t.kind, t.text))) shouldBe Right(List(
      (StdTokenKind.StringLit, "hello"),
    ))
  }

  // --- Custom StdLexer: comments ---

  "custom skip with line comments" in {
    object CommentLexer extends StdLexer:
      delimiters ++= List("+")
      override protected def skip(using ctx: LexCtx): Unit =
        skipWhitespace("//")

    val result = CommentLexer.tokenize("x // comment\n+ y")
    result.map(_.toList.map(t => (t.kind, t.text))) shouldBe Right(List(
      (StdTokenKind.Ident, "x"),
      (StdTokenKind.Delimiter, "+"),
      (StdTokenKind.Ident, "y"),
    ))
  }
