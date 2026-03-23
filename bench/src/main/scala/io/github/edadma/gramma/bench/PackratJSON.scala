package io.github.edadma.gramma.bench

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

// Scala parser combinators with packrat (memoized) token parsing

object PackratJSON extends StandardTokenParsers with PackratParsers:

  override val lexical: StdLexical = new StdLexical:
    delimiters ++= List("{", "}", "[", "]", ":", ",")
    reserved ++= List("true", "false", "null")

  // --- Packrat parser rules ---

  lazy val value: PackratParser[JValue] =
    obj | arr | stringVal | numberVal | boolVal | nullVal

  lazy val stringVal: PackratParser[JValue] =
    stringLit ^^ { s => JString(s) }

  lazy val numberVal: PackratParser[JValue] =
    numericLit ^^ { n => JNumber(n.toDouble) }

  lazy val boolVal: PackratParser[JValue] =
    keyword("true") ^^^ JBool(true) |
      keyword("false") ^^^ JBool(false)

  lazy val nullVal: PackratParser[JValue] =
    keyword("null") ^^^ JNull

  lazy val arr: PackratParser[JValue] =
    "[" ~> repsep(value, ",") <~ "]" ^^ { elems => JArray(elems) }

  lazy val obj: PackratParser[JValue] =
    "{" ~> repsep(field, ",") <~ "}" ^^ { fields => JObject(fields) }

  lazy val field: PackratParser[(String, JValue)] =
    stringLit ~ (":" ~> value) ^^ { case k ~ v => (k, v) }

  def parse(input: String): Either[String, JValue] =
    val tokens = new PackratReader(new lexical.Scanner(input))
    phrase(value)(tokens) match
      case Success(result, _)  => Right(result)
      case NoSuccess.I(msg, _) => Left(msg)
