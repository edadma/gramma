package io.github.edadma.gramma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class JSONTests extends AnyFreeSpec with Matchers:

  // --- JSON AST ---

  sealed trait JValue
  case class JObject(fields: List[(String, JValue)]) extends JValue
  case class JArray(elements: List[JValue]) extends JValue
  case class JString(value: String) extends JValue
  case class JNumber(value: Double) extends JValue
  case class JBool(value: Boolean) extends JValue
  case object JNull extends JValue

  // --- Token types ---

  enum TokenKind:
    case StringLit, NumberLit
    case True, False, Null
    case LBrace, RBrace, LBracket, RBracket, Colon, Comma

  case class Token(kind: TokenKind, text: String, pos: Pos)

  // --- Lexer ---

  object JSONLexer extends Lexers:
    def nextToken(using ctx: LexCtx): P[Token] =
      whitespace
      if ctx.atEnd then
        ctx.ok = false
        fail
      else
        val pos = ctx.capturePos()
        stringLit('"', '\\') ^^ { text => Token(TokenKind.StringLit, text, pos) } |
          decimalLit ^^ { text => Token(TokenKind.NumberLit, text, pos) } |
          str("true") ^^ { _ => Token(TokenKind.True, "true", pos) } |
          str("false") ^^ { _ => Token(TokenKind.False, "false", pos) } |
          str("null") ^^ { _ => Token(TokenKind.Null, "null", pos) } |
          char('{') ^^ { _ => Token(TokenKind.LBrace, "{", pos) } |
          char('}') ^^ { _ => Token(TokenKind.RBrace, "}", pos) } |
          char('[') ^^ { _ => Token(TokenKind.LBracket, "[", pos) } |
          char(']') ^^ { _ => Token(TokenKind.RBracket, "]", pos) } |
          char(':') ^^ { _ => Token(TokenKind.Colon, ":", pos) } |
          char(',') ^^ { _ => Token(TokenKind.Comma, ",", pos) }

  // --- Parser ---

  object JSONParser extends TokenParsers[Token]:
    def tokenPos(token: Token): Pos = token.pos

    private def tok(kind: TokenKind, msg: String)(using ctx: ParseCtx): P[Token] =
      accept(_.kind == kind, msg)

    def stringLit(using ctx: ParseCtx): P[String] =
      tok(TokenKind.StringLit, "string") ^^ { _.text }

    def numberLit(using ctx: ParseCtx): P[Double] =
      tok(TokenKind.NumberLit, "number") ^^ { _.text.toDouble }

    def value(using ctx: ParseCtx): P[JValue] =
      jObject | jArray | jString | jNumber | jBool | jNull

    def jString(using ctx: ParseCtx): P[JValue] =
      stringLit ^^ { s => JString(s) }

    def jNumber(using ctx: ParseCtx): P[JValue] =
      numberLit ^^ { n => JNumber(n) }

    def jBool(using ctx: ParseCtx): P[JValue] =
      tok(TokenKind.True, "true") ^^ { _ => JBool(true) } |
        tok(TokenKind.False, "false") ^^ { _ => JBool(false) }

    def jNull(using ctx: ParseCtx): P[JValue] =
      tok(TokenKind.Null, "null") ^^ { _ => JNull }

    def jArray(using ctx: ParseCtx): P[JValue] =
      tok(TokenKind.LBracket, "'['") ~> repsep(value, tok(TokenKind.Comma, "','")) <~ tok(TokenKind.RBracket, "']'") ^^ { elems =>
        JArray(elems)
      }

    def jObject(using ctx: ParseCtx): P[JValue] =
      tok(TokenKind.LBrace, "'{'") ~> repsep(field, tok(TokenKind.Comma, "','")) <~ tok(TokenKind.RBrace, "'}'") ^^ { fields =>
        JObject(fields)
      }

    def field(using ctx: ParseCtx): P[(String, JValue)] =
      stringLit ~ (tok(TokenKind.Colon, "':'") ~> value) ^^ {
        case key ~ v => (key, v)
      }

    def parseTokens(tokens: Array[Token]): Either[ParseError, JValue] =
      parse(tokens, value)

  // --- Helper ---

  def parseJSON(input: String): Either[ParseError, JValue] =
    for
      tokens <- JSONLexer.tokenize(input, JSONLexer.nextToken)
      ast    <- JSONParser.parseTokens(tokens)
    yield ast

  // --- Primitives ---

  "parse null" in {
    parseJSON("null") shouldBe Right(JNull)
  }

  "parse true" in {
    parseJSON("true") shouldBe Right(JBool(true))
  }

  "parse false" in {
    parseJSON("false") shouldBe Right(JBool(false))
  }

  "parse integer number" in {
    parseJSON("42") shouldBe Right(JNumber(42.0))
  }

  "parse decimal number" in {
    parseJSON("3.14") shouldBe Right(JNumber(3.14))
  }

  "parse simple string" in {
    parseJSON("\"hello\"") shouldBe Right(JString("hello"))
  }

  "parse string with escapes" in {
    parseJSON("\"hello\\nworld\"") shouldBe Right(JString("hello\nworld"))
  }

  "parse empty string" in {
    parseJSON("\"\"") shouldBe Right(JString(""))
  }

  // --- Arrays ---

  "parse empty array" in {
    parseJSON("[]") shouldBe Right(JArray(Nil))
  }

  "parse array with one element" in {
    parseJSON("[1]") shouldBe Right(JArray(List(JNumber(1.0))))
  }

  "parse array with multiple elements" in {
    parseJSON("[1, 2, 3]") shouldBe Right(JArray(List(JNumber(1.0), JNumber(2.0), JNumber(3.0))))
  }

  "parse array with mixed types" in {
    parseJSON("[1, \"two\", true, null]") shouldBe Right(
      JArray(List(JNumber(1.0), JString("two"), JBool(true), JNull))
    )
  }

  "parse nested arrays" in {
    parseJSON("[[1, 2], [3, 4]]") shouldBe Right(
      JArray(List(
        JArray(List(JNumber(1.0), JNumber(2.0))),
        JArray(List(JNumber(3.0), JNumber(4.0))),
      ))
    )
  }

  // --- Objects ---

  "parse empty object" in {
    parseJSON("{}") shouldBe Right(JObject(Nil))
  }

  "parse object with one field" in {
    parseJSON("{\"name\": \"Alice\"}") shouldBe Right(
      JObject(List("name" -> JString("Alice")))
    )
  }

  "parse object with multiple fields" in {
    parseJSON("{\"name\": \"Alice\", \"age\": 30}") shouldBe Right(
      JObject(List("name" -> JString("Alice"), "age" -> JNumber(30.0)))
    )
  }

  "parse nested object" in {
    parseJSON("{\"user\": {\"name\": \"Alice\"}}") shouldBe Right(
      JObject(List("user" -> JObject(List("name" -> JString("Alice")))))
    )
  }

  "parse object with array value" in {
    parseJSON("{\"nums\": [1, 2, 3]}") shouldBe Right(
      JObject(List("nums" -> JArray(List(JNumber(1.0), JNumber(2.0), JNumber(3.0)))))
    )
  }

  // --- Complex ---

  "parse complex nested structure" in {
    val input = """{"users": [{"name": "Alice", "active": true}, {"name": "Bob", "active": false}], "count": 2}"""
    parseJSON(input) shouldBe Right(
      JObject(List(
        "users" -> JArray(List(
          JObject(List("name" -> JString("Alice"), "active" -> JBool(true))),
          JObject(List("name" -> JString("Bob"), "active" -> JBool(false))),
        )),
        "count" -> JNumber(2.0),
      ))
    )
  }

  "parse with whitespace and newlines" in {
    val input =
      """{
        |  "name": "Alice",
        |  "age": 30
        |}""".stripMargin
    parseJSON(input) shouldBe Right(
      JObject(List("name" -> JString("Alice"), "age" -> JNumber(30.0)))
    )
  }

  // --- Error reporting ---

  "error on trailing comma in array" in {
    val result = parseJSON("[1, 2,]")
    result shouldBe a[Left[?, ?]]
  }

  "error on trailing comma in object" in {
    val result = parseJSON("{\"a\": 1,}")
    result shouldBe a[Left[?, ?]]
  }

  "error on missing colon" in {
    val result = parseJSON("{\"a\" 1}")
    result match
      case Left(err) => err.msg should include("':'")
      case Right(_)  => fail("expected parse error")
  }

  "error on missing closing bracket" in {
    val result = parseJSON("[1, 2")
    result match
      case Left(err) => err.msg should include("']'")
      case Right(_)  => fail("expected parse error")
  }

  "error on missing closing brace" in {
    val result = parseJSON("{\"a\": 1")
    result match
      case Left(err) => err.msg should include("'}'")
      case Right(_)  => fail("expected parse error")
  }

  "error reports correct position" in {
    val result = parseJSON("{\"a\": }")
    result match
      case Left(err) =>
        err.pos.line shouldBe 1
        err.pos.col shouldBe 7
      case Right(_) => fail("expected parse error")
  }

  "multiline error reports correct line" in {
    val input =
      """{
        |  "a": 1,
        |  "b": ,
        |}""".stripMargin
    val result = parseJSON(input)
    result match
      case Left(err) =>
        err.pos.line shouldBe 3
      case Right(_) => fail("expected parse error")
  }

  "lexer error on invalid token" in {
    val result = parseJSON("[1, @]")
    result shouldBe a[Left[?, ?]]
  }
