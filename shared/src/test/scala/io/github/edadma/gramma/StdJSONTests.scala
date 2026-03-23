package io.github.edadma.gramma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class StdJSONTests extends AnyFreeSpec with Matchers:

  // --- AST ---

  sealed trait JValue
  case class JObject(fields: List[(String, JValue)]) extends JValue
  case class JArray(elements: List[JValue]) extends JValue
  case class JString(value: String) extends JValue
  case class JNumber(value: Double) extends JValue
  case class JBool(value: Boolean) extends JValue
  case object JNull extends JValue

  // --- Lexer: just declare delimiters and keywords ---

  object JSONLexer extends StdLexer:
    delimiters ++= List("{", "}", "[", "]", ":", ",")
    reserved ++= List("true", "false", "null")

  // --- Parser: use ident, stringLit, numericLit, keyword, delimiter ---

  object JSONParser extends StdParsers(JSONLexer):

    def value(using ctx: ParseCtx): P[JValue] =
      obj | arr | jString | jNumber | jBool | jNull

    def jString(using ctx: ParseCtx): P[JValue] =
      stringLit ^^ { s => JString(s) }

    def jNumber(using ctx: ParseCtx): P[JValue] =
      numericLit ^^ { n => JNumber(n.toDouble) }

    def jBool(using ctx: ParseCtx): P[JValue] =
      keyword("true") ^^ { _ => JBool(true) } |
        keyword("false") ^^ { _ => JBool(false) }

    def jNull(using ctx: ParseCtx): P[JValue] =
      keyword("null") ^^ { _ => JNull }

    def arr(using ctx: ParseCtx): P[JValue] =
      delimiter("[") ~> repsep(value, delimiter(",")) <~ delimiter("]") ^^ { elems => JArray(elems) }

    def obj(using ctx: ParseCtx): P[JValue] =
      delimiter("{") ~> repsep(field, delimiter(",")) <~ delimiter("}") ^^ { fields => JObject(fields) }

    def field(using ctx: ParseCtx): P[(String, JValue)] =
      stringLit ~ (delimiter(":") ~> value) ^^ { case k ~ v => (k, v) }

  // --- Helper ---

  def parseJSON(input: String): Either[ParseError, JValue] =
    JSONParser.parseSource(input)(JSONParser.value)

  // --- Tests ---

  "parse null" in {
    parseJSON("null") shouldBe Right(JNull)
  }

  "parse true" in {
    parseJSON("true") shouldBe Right(JBool(true))
  }

  "parse false" in {
    parseJSON("false") shouldBe Right(JBool(false))
  }

  "parse integer" in {
    parseJSON("42") shouldBe Right(JNumber(42.0))
  }

  "parse decimal" in {
    parseJSON("3.14") shouldBe Right(JNumber(3.14))
  }

  "parse string" in {
    parseJSON("\"hello\"") shouldBe Right(JString("hello"))
  }

  "parse string with escapes" in {
    parseJSON("\"hello\\nworld\"") shouldBe Right(JString("hello\nworld"))
  }

  "parse empty array" in {
    parseJSON("[]") shouldBe Right(JArray(Nil))
  }

  "parse array" in {
    parseJSON("[1, 2, 3]") shouldBe Right(JArray(List(JNumber(1.0), JNumber(2.0), JNumber(3.0))))
  }

  "parse mixed array" in {
    parseJSON("[1, \"two\", true, null]") shouldBe Right(
      JArray(List(JNumber(1.0), JString("two"), JBool(true), JNull))
    )
  }

  "parse empty object" in {
    parseJSON("{}") shouldBe Right(JObject(Nil))
  }

  "parse object" in {
    parseJSON("{\"name\": \"Alice\", \"age\": 30}") shouldBe Right(
      JObject(List("name" -> JString("Alice"), "age" -> JNumber(30.0)))
    )
  }

  "parse nested" in {
    parseJSON("{\"users\": [{\"name\": \"Alice\"}, {\"name\": \"Bob\"}]}") shouldBe Right(
      JObject(List("users" -> JArray(List(
        JObject(List("name" -> JString("Alice"))),
        JObject(List("name" -> JString("Bob"))),
      ))))
    )
  }

  "parse complex" in {
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

  "error on trailing comma" in {
    parseJSON("[1, 2,]") shouldBe a[Left[?, ?]]
  }

  "error on missing colon" in {
    val result = parseJSON("{\"a\" 1}")
    result match
      case Left(err) => err.msg should include("':'")
      case Right(_)  => fail("expected error")
  }

  "error on invalid character" in {
    parseJSON("[1, @]") shouldBe a[Left[?, ?]]
  }

  "error reports position" in {
    val result = parseJSON("{\"a\": }")
    result match
      case Left(err) => err.pos.col shouldBe 7
      case Right(_)  => fail("expected error")
  }
