package io.github.edadma.gramma

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

// --- Shared JSON AST ---

private sealed trait JValue
private case class JObject(fields: List[(String, JValue)]) extends JValue
private case class JArray(elements: List[JValue]) extends JValue
private case class JString(value: String) extends JValue
private case class JNumber(value: Double) extends JValue
private case class JBool(value: Boolean) extends JValue
private case object JNull extends JValue

// --- Gramma StdLexer/StdParsers JSON ---

private object GrammaLexer extends StdLexer:
  delimiters ++= List("{", "}", "[", "]", ":", ",")
  reserved ++= List("true", "false", "null")

private object GrammaParser extends StdParsers(GrammaLexer):
  def value(using ctx: ParseCtx): P[JValue] =
    obj | arr | stringLit ^^ (JString(_)) |
      numericLit ^^ (n => JNumber(n.toDouble)) |
      keyword("true") ^^ (_ => JBool(true)) |
      keyword("false") ^^ (_ => JBool(false)) |
      keyword("null") ^^ (_ => JNull)

  def arr(using ctx: ParseCtx): P[JValue] =
    delimiter("[") ~> repsep(value, delimiter(",")) <~ delimiter("]") ^^ (JArray(_))

  def obj(using ctx: ParseCtx): P[JValue] =
    delimiter("{") ~> repsep(field, delimiter(",")) <~ delimiter("}") ^^ (JObject(_))

  def field(using ctx: ParseCtx): P[(String, JValue)] =
    stringLit ~ (delimiter(":") ~> value) ^^ { case k ~ v => (k, v) }

// --- Scala parser combinators JSON ---

private object ScalaCombParser extends StandardTokenParsers:
  override val lexical: StdLexical = new StdLexical:
    delimiters ++= List("{", "}", "[", "]", ":", ",")
    reserved ++= List("true", "false", "null")

  def value: Parser[JValue] =
    obj | arr | stringVal | numberVal | boolVal | nullVal

  def stringVal: Parser[JValue] = stringLit ^^ (JString(_))
  def numberVal: Parser[JValue] = numericLit ^^ (n => JNumber(n.toDouble))
  def boolVal: Parser[JValue] = keyword("true") ^^^ JBool(true) | keyword("false") ^^^ JBool(false)
  def nullVal: Parser[JValue] = keyword("null") ^^^ JNull

  def arr: Parser[JValue] = "[" ~> repsep(value, ",") <~ "]" ^^ (JArray(_))
  def obj: Parser[JValue] = "{" ~> repsep(field, ",") <~ "}" ^^ (JObject(_))
  def field: Parser[(String, JValue)] = stringLit ~ (":" ~> value) ^^ { case k ~ v => (k, v) }

  def parse(input: String): Either[String, JValue] =
    phrase(value)(new lexical.Scanner(input)) match
      case Success(result, _)  => Right(result)
      case NoSuccess.I(msg, _) => Left(msg)

// --- Fastparse JSON ---

private object FastparseParser:
  import fastparse.*
  import fastparse.NoWhitespace.*

  def ws[$: P]: P[Unit] = P(CharsWhileIn(" \t\r\n", 0))
  def stringChars(c: Char): Boolean = c != '"' && c != '\\'
  def hexDigit[$: P]: P[Unit] = P(CharIn("0-9a-fA-F"))
  def unicodeEscape[$: P]: P[Unit] = P("u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit)
  def escape[$: P]: P[Unit] = P("\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape))
  def strChars[$: P]: P[Unit] = P(CharsWhile(stringChars))
  def string[$: P]: P[String] = P("\"" ~/ (strChars | escape).rep.! ~ "\"")
  def digits[$: P]: P[Unit] = P(CharsWhileIn("0-9"))
  def exponent[$: P]: P[Unit] = P(CharIn("eE") ~ CharIn("+\\-").? ~ digits)
  def fractional[$: P]: P[Unit] = P("." ~ digits)
  def integral[$: P]: P[Unit] = P("0" | CharIn("1-9") ~ digits.?)
  def number[$: P]: P[Double] = P(CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.?).!.map(_.toDouble)

  def `null`[$: P]: P[JValue] = P("null").map(_ => JNull)
  def `true`[$: P]: P[JValue] = P("true").map(_ => JBool(true))
  def `false`[$: P]: P[JValue] = P("false").map(_ => JBool(false))
  def jString[$: P]: P[JValue] = string.map(JString(_))
  def jNumber[$: P]: P[JValue] = number.map(JNumber(_))

  def arr[$: P]: P[JValue] = P("[" ~/ ws ~ value.rep(sep = ws ~ "," ~ ws) ~ ws ~ "]").map(elems => JArray(elems.toList))
  def obj[$: P]: P[JValue] = P("{" ~/ ws ~ field.rep(sep = ws ~ "," ~ ws) ~ ws ~ "}").map(fields => JObject(fields.toList))
  def field[$: P]: P[(String, JValue)] = P(string ~ ws ~ ":" ~ ws ~ value)
  def value[$: P]: P[JValue] = P(ws ~ (obj | arr | jString | jNumber | `true` | `false` | `null`) ~ ws)
  def jsonExpr[$: P]: P[JValue] = P(value ~ End)

  def parse(input: String): Either[String, JValue] =
    fastparse.parse(input, jsonExpr(using _)) match
      case fastparse.Parsed.Success(result, _) => Right(result)
      case f: fastparse.Parsed.Failure         => Left(f.msg)

// --- Benchmark runner ---

private def bench(name: String, input: String, warmup: Int, iters: Int)(f: String => Any): Unit =
  // Warmup: run until timing stabilizes (for JIT), minimum `warmup` iterations
  var i = 0
  while i < warmup do { f(input); i += 1 }
  // Additional JIT warmup: run timed rounds until two consecutive rounds are within 10%
  var prevRate = 0.0
  var stable = false
  while !stable do
    val tw0 = System.currentTimeMillis()
    i = 0; while i < warmup do { f(input); i += 1 }
    val tw1 = System.currentTimeMillis()
    val rate = if tw1 > tw0 then warmup.toDouble / (tw1 - tw0) else 0.0
    if prevRate > 0 && rate > 0 && Math.abs(rate - prevRate) / prevRate < 0.10 then
      stable = true
    prevRate = rate

  // Measure
  val t0 = System.currentTimeMillis()
  i = 0
  while i < iters do { f(input); i += 1 }
  val elapsed = System.currentTimeMillis() - t0
  val usPerOp = if elapsed > 0 then (elapsed * 1000.0) / iters else 0.0
  val opsPerSec = if elapsed > 0 then (iters * 1000.0) / elapsed else 0.0
  println(f"  $name%-14s ${usPerOp}%8.1f µs/op  ${opsPerSec}%12.0f ops/s")

@main def run(args: String*): Unit =
  val small = """{"name": "Alice", "age": 30, "active": true}"""

  val medium =
    """{"users": [""" +
      (1 to 20).map(i => s"""{"id": $i, "name": "user$i", "email": "user$i@example.com", "active": ${i % 2 == 0}}""").mkString(", ") +
      """], "total": 20, "page": 1}"""

  val large =
    "{\n  \"data\": [\n" +
      (1 to 100).map { i =>
        s"""    {"id": $i, "name": "item$i", "tags": ["tag${i % 5}", "tag${i % 3}"], "meta": {"created": "2024-01-0${(i % 9) + 1}", "score": ${i * 1.5}}}"""
      }.mkString(",\n") +
      "\n  ],\n  \"status\": \"ok\"\n}"

  println(s"JSON parsing benchmark ($platform)")
  println(f"  small:  ${small.length}%,d chars")
  println(f"  medium: ${medium.length}%,d chars")
  println(f"  large:  ${large.length}%,d chars")
  println()

  // Verify all parsers produce correct results
  assert(GrammaParser.parseSource(small)(GrammaParser.value).isRight, "gramma small failed")
  assert(ScalaCombParser.parse(small).isRight, "scala-comb small failed")
  assert(FastparseParser.parse(small).isRight, "fastparse small failed")
  assert(GrammaParser.parseSource(large)(GrammaParser.value).isRight, "gramma large failed")

  val warmup = 1000
  val iters = 5000

  println("Small:")
  bench("gramma", small, warmup, iters * 10)(s => GrammaParser.parseSource(s)(GrammaParser.value))
  bench("fastparse", small, warmup, iters * 10)(s => FastparseParser.parse(s))
  bench("scala-comb", small, warmup, iters * 10)(s => ScalaCombParser.parse(s))

  println("Medium:")
  bench("gramma", medium, warmup, iters)(s => GrammaParser.parseSource(s)(GrammaParser.value))
  bench("fastparse", medium, warmup, iters)(s => FastparseParser.parse(s))
  bench("scala-comb", medium, warmup, iters)(s => ScalaCombParser.parse(s))

  println("Large:")
  bench("gramma", large, warmup, iters)(s => GrammaParser.parseSource(s)(GrammaParser.value))
  bench("fastparse", large, warmup, iters)(s => FastparseParser.parse(s))
  bench("scala-comb", large, warmup, iters)(s => ScalaCombParser.parse(s))
