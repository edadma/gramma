package io.github.edadma.gramma.bench

import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input.CharArrayReader

// Scala standard parser combinators — token-based JSON parser

object ScalaCombJSON extends StandardTokenParsers:

  // Extend StdLexical to handle JSON-specific tokens
  override val lexical: StdLexical = new StdLexical:
    // Add JSON punctuation as delimiters
    delimiters ++= List("{", "}", "[", "]", ":", ",")
    // Add JSON keywords
    reserved ++= List("true", "false", "null")

    // Override token to handle negative numbers (StdLexical doesn't by default)
    // StdLexical already handles: identifiers, string literals, numeric literals, delimiters, reserved words

  // --- Parser rules ---

  def value: Parser[JValue] =
    obj | arr | stringVal | numberVal | boolVal | nullVal

  def stringVal: Parser[JValue] =
    stringLit ^^ { s => JString(s) }

  def numberVal: Parser[JValue] =
    numericLit ^^ { n => JNumber(n.toDouble) }

  def boolVal: Parser[JValue] =
    keyword("true") ^^^ JBool(true) |
      keyword("false") ^^^ JBool(false)

  def nullVal: Parser[JValue] =
    keyword("null") ^^^ JNull

  def arr: Parser[JValue] =
    "[" ~> repsep(value, ",") <~ "]" ^^ { elems => JArray(elems) }

  def obj: Parser[JValue] =
    "{" ~> repsep(field, ",") <~ "}" ^^ { fields => JObject(fields) }

  def field: Parser[(String, JValue)] =
    stringLit ~ (":" ~> value) ^^ { case k ~ v => (k, v) }

  def parse(input: String): Either[String, JValue] =
    val tokens = new lexical.Scanner(input)
    phrase(value)(tokens) match
      case Success(result, _)   => Right(result)
      case NoSuccess.I(msg, _)  => Left(msg)
