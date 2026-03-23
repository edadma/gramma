package io.github.edadma.gramma.bench

import fastparse.*
import fastparse.NoWhitespace.*

object FastparseJSON:

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
      case Parsed.Success(result, _) => Right(result)
      case f: Parsed.Failure         => Left(f.msg)
