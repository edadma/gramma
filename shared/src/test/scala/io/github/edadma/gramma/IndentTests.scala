package io.github.edadma.gramma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IndentTests extends AnyFreeSpec with Matchers:

  object IndentLexer extends StdLexer:
    override protected def indentSensitive: Boolean = true
    delimiters ++= List("(", ")", "[", "]", "{", "}", ":", ",", "+", "-", "=")
    reserved ++= List("if", "else", "def", "return", "pass", "for", "in", "while", "class")
    // In indent mode, skip only handles inline whitespace between tokens on the same line.
    // The tokenizeIndent method handles newlines and indentation.


  def lex(input: String): Either[ParseError, List[(StdTokenKind, String)]] =
    IndentLexer.tokenize(input).map(_.toList.map(t => (t.kind, t.text)))

  def lexKinds(input: String): Either[ParseError, List[StdTokenKind]] =
    IndentLexer.tokenize(input).map(_.toList.map(_.kind))

  import StdTokenKind.*

  // --- Basic indentation ---

  "single line, no indentation" in {
    lex("x") shouldBe Right(List((Ident, "x")))
  }

  "two lines, same indentation" in {
    lex("x\ny") shouldBe Right(List(
      (Ident, "x"),
      (Newline, ""),
      (Ident, "y"),
    ))
  }

  "indent and dedent" in {
    val input = "x\n  y\nz"
    lex(input) shouldBe Right(List(
      (Ident, "x"),
      (Indent, ""),
      (Ident, "y"),
      (Dedent, ""),
      (Newline, ""),
      (Ident, "z"),
    ))
  }

  "multiple statements at same indent" in {
    val input = "x\n  y\n  z\nw"
    lex(input) shouldBe Right(List(
      (Ident, "x"),
      (Indent, ""),
      (Ident, "y"),
      (Newline, ""),
      (Ident, "z"),
      (Dedent, ""),
      (Newline, ""),
      (Ident, "w"),
    ))
  }

  "nested indentation" in {
    val input = "a\n  b\n    c\nd"
    lex(input) shouldBe Right(List(
      (Ident, "a"),
      (Indent, ""),
      (Ident, "b"),
      (Indent, ""),
      (Ident, "c"),
      (Dedent, ""),
      (Dedent, ""),
      (Newline, ""),
      (Ident, "d"),
    ))
  }

  "double dedent" in {
    val input = "a\n  b\n    c\n  d\ne"
    lex(input) shouldBe Right(List(
      (Ident, "a"),
      (Indent, ""),
      (Ident, "b"),
      (Indent, ""),
      (Ident, "c"),
      (Dedent, ""),
      (Newline, ""),
      (Ident, "d"),
      (Dedent, ""),
      (Newline, ""),
      (Ident, "e"),
    ))
  }

  // --- Dedent at EOF ---

  "dedent emitted at EOF" in {
    val input = "a\n  b"
    lex(input) shouldBe Right(List(
      (Ident, "a"),
      (Indent, ""),
      (Ident, "b"),
      (Dedent, ""),
    ))
  }

  "multiple dedents at EOF" in {
    val input = "a\n  b\n    c"
    lex(input) shouldBe Right(List(
      (Ident, "a"),
      (Indent, ""),
      (Ident, "b"),
      (Indent, ""),
      (Ident, "c"),
      (Dedent, ""),
      (Dedent, ""),
    ))
  }

  // --- Blank lines ---

  "blank lines are ignored" in {
    val input = "x\n\n  y"
    lex(input) shouldBe Right(List(
      (Ident, "x"),
      (Indent, ""),
      (Ident, "y"),
      (Dedent, ""),
    ))
  }

  "whitespace-only lines are ignored" in {
    val input = "x\n   \n  y"
    lex(input) shouldBe Right(List(
      (Ident, "x"),
      (Indent, ""),
      (Ident, "y"),
      (Dedent, ""),
    ))
  }

  "multiple blank lines between code" in {
    val input = "x\n\n\n\ny"
    lex(input) shouldBe Right(List(
      (Ident, "x"),
      (Newline, ""),
      (Ident, "y"),
    ))
  }

  // --- Comment lines ---

  "comment-only lines are ignored" in {
    val input = "x\n# comment\n  y"
    lex(input) shouldBe Right(List(
      (Ident, "x"),
      (Indent, ""),
      (Ident, "y"),
      (Dedent, ""),
    ))
  }

  "indented comment line is ignored" in {
    val input = "x\n  y\n  # comment\n  z"
    lex(input) shouldBe Right(List(
      (Ident, "x"),
      (Indent, ""),
      (Ident, "y"),
      (Newline, ""),
      (Ident, "z"),
      (Dedent, ""),
    ))
  }

  // --- Brackets suppress indentation ---

  "brackets suppress indent/dedent" in {
    val input = "f(\n  x,\n  y\n)"
    lex(input) shouldBe Right(List(
      (Ident, "f"),
      (Delimiter, "("),
      (Ident, "x"),
      (Delimiter, ","),
      (Ident, "y"),
      (Delimiter, ")"),
    ))
  }

  "square brackets suppress indentation" in {
    val input = "[\n  1,\n  2\n]"
    lex(input) shouldBe Right(List(
      (Delimiter, "["),
      (NumericLit, "1"),
      (Delimiter, ","),
      (NumericLit, "2"),
      (Delimiter, "]"),
    ))
  }

  "curly braces suppress indentation" in {
    val input = "{\n  x\n}"
    lex(input) shouldBe Right(List(
      (Delimiter, "{"),
      (Ident, "x"),
      (Delimiter, "}"),
    ))
  }

  "nested brackets" in {
    val input = "f(\n  g(\n    x\n  )\n)"
    lex(input) shouldBe Right(List(
      (Ident, "f"),
      (Delimiter, "("),
      (Ident, "g"),
      (Delimiter, "("),
      (Ident, "x"),
      (Delimiter, ")"),
      (Delimiter, ")"),
    ))
  }

  "indentation resumes after brackets" in {
    val input = "a\n  f(x)\n  b"
    lex(input) shouldBe Right(List(
      (Ident, "a"),
      (Indent, ""),
      (Ident, "f"),
      (Delimiter, "("),
      (Ident, "x"),
      (Delimiter, ")"),
      (Newline, ""),
      (Ident, "b"),
      (Dedent, ""),
    ))
  }

  // --- Inconsistent indentation ---

  "inconsistent dedent is an error" in {
    val input = "a\n    b\n  c" // indent by 4, then dedent to 2 (not on stack)
    lex(input) shouldBe a[Left[?, ?]]
  }

  // --- Real-world patterns ---

  "if/else block" in {
    val input = "if x:\n  y\nelse:\n  z"
    lexKinds(input) shouldBe Right(List(
      Keyword, Ident, Delimiter, // if x :
      Indent, Ident, Dedent,     // y
      Newline, Keyword, Delimiter, // else :
      Indent, Ident, Dedent,     // z
    ))
  }

  "function definition" in {
    val input = "def f(x):\n  return x + 1"
    lexKinds(input) shouldBe Right(List(
      Keyword, Ident, Delimiter, Ident, Delimiter, Delimiter, // def f ( x ) :
      Indent, Keyword, Ident, Delimiter, NumericLit, Dedent,  // return x + 1
    ))
  }

  "nested blocks" in {
    val input = "def f(x):\n  if x:\n    return 1\n  return 0"
    lexKinds(input) shouldBe Right(List(
      Keyword, Ident, Delimiter, Ident, Delimiter, Delimiter, // def f ( x ) :
      Indent,                                                  // block start
      Keyword, Ident, Delimiter,                               // if x :
      Indent, Keyword, NumericLit, Dedent,                     // return 1
      Newline,                                                 // same indent
      Keyword, NumericLit,                                     // return 0
      Dedent,                                                  // block end
    ))
  }

  // --- Edge cases ---

  "empty input" in {
    lex("") shouldBe Right(Nil)
  }

  "only whitespace" in {
    lex("   ") shouldBe Right(Nil)
  }

  "only newlines" in {
    lex("\n\n\n") shouldBe Right(Nil)
  }

  "only comments" in {
    lex("# comment\n# another") shouldBe Right(Nil)
  }

  "trailing newline" in {
    lex("x\n") shouldBe Right(List((Ident, "x")))
  }

  "trailing newline after indent" in {
    lex("x\n  y\n") shouldBe Right(List(
      (Ident, "x"),
      (Indent, ""),
      (Ident, "y"),
      (Dedent, ""),
    ))
  }

  "multiple tokens on one line" in {
    val input = "x + y\n  a + b"
    lex(input) shouldBe Right(List(
      (Ident, "x"),
      (Delimiter, "+"),
      (Ident, "y"),
      (Indent, ""),
      (Ident, "a"),
      (Delimiter, "+"),
      (Ident, "b"),
      (Dedent, ""),
    ))
  }

  // --- Parser integration ---

  object IndentParser extends StdParsers(IndentLexer):
    def program(using ctx: ParseCtx): P[List[Any]] =
      rep1sep(statement, newline)

    def statement(using ctx: ParseCtx): P[Any] =
      ifStmt | (expr ^^ (x => x: Any))

    def ifStmt(using ctx: ParseCtx): P[Any] =
      (keyword("if") ~ expr ~ delimiter(":") ~ block(rep1sep(statement, newline))) ^^ { r => r }

    def expr(using ctx: ParseCtx): P[String] =
      ident | numericLit

  "parser: simple indented block" in {
    val input = "if x:\n  y\n  z"
    IndentParser.parseSource(input)(IndentParser.program) shouldBe a[Right[?, ?]]
  }

  "parser: nested blocks" in {
    val input = "if x:\n  if y:\n    z"
    IndentParser.parseSource(input)(IndentParser.program) shouldBe a[Right[?, ?]]
  }
