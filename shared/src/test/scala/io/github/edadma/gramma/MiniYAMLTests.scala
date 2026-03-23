package io.github.edadma.gramma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MiniYAMLTests extends AnyFreeSpec with Matchers:

  // --- YAML AST (same as JSON) ---

  sealed trait YValue
  case class YMap(fields: List[(String, YValue)]) extends YValue
  case class YList(elements: List[YValue]) extends YValue
  case class YString(value: String) extends YValue
  case class YNumber(value: Double) extends YValue
  case class YBool(value: Boolean) extends YValue
  case object YNull extends YValue

  // --- Lexer ---

  object YAMLLexer extends StdLexer:
    override protected def indentSensitive: Boolean = true
    delimiters ++= List(":", "-")
    reserved ++= List("true", "false", "null")

  // --- Parser ---
  // Grammar structured so committed choice works:
  // - Lists start with "-" (unambiguous)
  // - Maps: parse key, then require ":" (key consumed = committed to map)
  // - Scalars: numbers/bools/null tried first (don't start with ident),
  //   then quoted strings, then bare idents last (after map fails)

  object YAMLParser extends StdParsers(YAMLLexer):
    import scala.language.implicitConversions

    def document(using ctx: ParseCtx): P[YValue] =
      blockList | blockMap | inlineScalar

    def value(using ctx: ParseCtx): P[YValue] =
      blockList | blockMap | inlineScalar

    // Scalars that can't be confused with map keys
    def inlineScalar(using ctx: ParseCtx): P[YValue] =
      numericLit ^^ (n => YNumber(n.toDouble)) |
        keyword("true") ^^ (_ => YBool(true)) |
        keyword("false") ^^ (_ => YBool(false)) |
        keyword("null") ^^ (_ => YNull) |
        stringLit ^^ (s => YString(s)) |
        ident ^^ (s => YString(s)) // bare word — last, after map check fails

    def blockMap(using ctx: ParseCtx): P[YValue] =
      rep1sep(mapEntry, newline) ^^ (entries => YMap(entries))

    def mapEntry(using ctx: ParseCtx): P[(String, YValue)] =
      mapKey ~ (":" ~> mapValue) ^^ { case k ~ v => (k, v) }

    def mapKey(using ctx: ParseCtx): P[String] =
      ident | stringLit

    def mapValue(using ctx: ParseCtx): P[YValue] =
      inlineScalar | block(value)

    def blockList(using ctx: ParseCtx): P[YValue] =
      rep1sep(listItem, newline) ^^ (items => YList(items))

    def listItem(using ctx: ParseCtx): P[YValue] =
      "-" ~> (inlineScalar | block(value))

  // --- Helper ---

  def parse(input: String): Either[ParseError, YValue] =
    YAMLParser.parseSource(input)(YAMLParser.document)

  // --- Scalar values ---

  "parse integer" in {
    parse("42") shouldBe Right(YNumber(42.0))
  }

  "parse decimal" in {
    parse("3.14") shouldBe Right(YNumber(3.14))
  }

  "parse true" in {
    parse("true") shouldBe Right(YBool(true))
  }

  "parse false" in {
    parse("false") shouldBe Right(YBool(false))
  }

  "parse null" in {
    parse("null") shouldBe Right(YNull)
  }

  "parse quoted string" in {
    // Standalone quoted strings can't be top-level documents because
    // blockMap tries stringLit as a map key first (committed choice).
    // In real YAML, standalone scalars are rare — they're always values.
    parse("msg: \"hello world\"") shouldBe Right(YMap(List("msg" -> YString("hello world"))))
  }

  // --- Maps ---

  "single key-value pair" in {
    parse("name: Alice") shouldBe Right(YMap(List("name" -> YString("Alice"))))
  }

  "multiple key-value pairs" in {
    val input =
      """name: Alice
        |age: 30""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "name" -> YString("Alice"),
      "age" -> YNumber(30.0),
    )))
  }

  "map with boolean values" in {
    val input =
      """active: true
        |deleted: false""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "active" -> YBool(true),
      "deleted" -> YBool(false),
    )))
  }

  "map with null value" in {
    parse("value: null") shouldBe Right(YMap(List("value" -> YNull)))
  }

  // --- Nested maps ---

  "nested map" in {
    val input =
      """address:
        |  city: Springfield
        |  zip: 12345""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "address" -> YMap(List(
        "city" -> YString("Springfield"),
        "zip" -> YNumber(12345.0),
      ))
    )))
  }

  "deeply nested map" in {
    val input =
      """a:
        |  b:
        |    c: deep""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "a" -> YMap(List(
        "b" -> YMap(List(
          "c" -> YString("deep"),
        ))
      ))
    )))
  }

  // --- Lists ---

  "simple list" in {
    val input =
      """- one
        |- two
        |- three""".stripMargin
    parse(input) shouldBe Right(YList(List(
      YString("one"),
      YString("two"),
      YString("three"),
    )))
  }

  "list of numbers" in {
    val input =
      """- 1
        |- 2
        |- 3""".stripMargin
    parse(input) shouldBe Right(YList(List(
      YNumber(1.0),
      YNumber(2.0),
      YNumber(3.0),
    )))
  }

  "list of booleans" in {
    val input =
      """- true
        |- false""".stripMargin
    parse(input) shouldBe Right(YList(List(YBool(true), YBool(false))))
  }

  // --- Nested lists in maps ---

  "map with list value" in {
    val input =
      """hobbies:
        |  - reading
        |  - coding""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "hobbies" -> YList(List(
        YString("reading"),
        YString("coding"),
      ))
    )))
  }

  // --- Complex structures ---

  "full document" in {
    val input =
      """name: Alice
        |age: 30
        |active: true
        |address:
        |  city: Springfield
        |  zip: 12345
        |hobbies:
        |  - reading
        |  - coding""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "name" -> YString("Alice"),
      "age" -> YNumber(30.0),
      "active" -> YBool(true),
      "address" -> YMap(List(
        "city" -> YString("Springfield"),
        "zip" -> YNumber(12345.0),
      )),
      "hobbies" -> YList(List(
        YString("reading"),
        YString("coding"),
      )),
    )))
  }

  // --- Comments ---

  "comment-only lines between entries" in {
    val input =
      """name: Alice
        |# this is a comment
        |age: 30""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "name" -> YString("Alice"),
      "age" -> YNumber(30.0),
    )))
  }

  "comment before indented block" in {
    val input =
      """address:
        |  # address fields
        |  city: Springfield
        |  zip: 12345""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "address" -> YMap(List(
        "city" -> YString("Springfield"),
        "zip" -> YNumber(12345.0),
      ))
    )))
  }

  // --- Blank lines ---

  "blank lines between entries" in {
    val input =
      """name: Alice
        |
        |age: 30""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "name" -> YString("Alice"),
      "age" -> YNumber(30.0),
    )))
  }

  "blank lines in indented block" in {
    val input =
      """address:
        |  city: Springfield
        |
        |  zip: 12345""".stripMargin
    parse(input) shouldBe Right(YMap(List(
      "address" -> YMap(List(
        "city" -> YString("Springfield"),
        "zip" -> YNumber(12345.0),
      ))
    )))
  }

  // --- Edge cases ---

  "trailing newline" in {
    parse("name: Alice\n") shouldBe Right(YMap(List("name" -> YString("Alice"))))
  }

  "quoted key" in {
    parse("\"full name\": Alice") shouldBe Right(YMap(List("full name" -> YString("Alice"))))
  }

  "numeric value in map" in {
    parse("port: 8080") shouldBe Right(YMap(List("port" -> YNumber(8080.0))))
  }

  "quoted value in map" in {
    parse("msg: \"hello world\"") shouldBe Right(YMap(List("msg" -> YString("hello world"))))
  }
