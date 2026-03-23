package io.github.edadma.gramma.bench

import io.github.edadma.gramma.*

// --- JSON AST (shared by both parsers) ---

sealed trait JValue
case class JObject(fields: List[(String, JValue)]) extends JValue
case class JArray(elements: List[JValue]) extends JValue
case class JString(value: String) extends JValue
case class JNumber(value: Double) extends JValue
case class JBool(value: Boolean) extends JValue
case object JNull extends JValue

// --- Gramma JSON implementation ---

enum GTokenKind:
  case StringLit, NumberLit
  case True, False, Null
  case LBrace, RBrace, LBracket, RBracket, Colon, Comma

case class GToken(kind: GTokenKind, text: String, pos: Pos)

object GrammaJSONLexer extends Lexers:
  def nextToken(using ctx: LexCtx): P[GToken] =
    whitespace
    if ctx.atEnd then
      ctx.ok = false
      fail
    else
      val pos = ctx.capturePos()
      stringLit('"', '\\') ^^ { text => GToken(GTokenKind.StringLit, text, pos) } |
        decimalLit ^^ { text => GToken(GTokenKind.NumberLit, text, pos) } |
        str("true") ^^ { _ => GToken(GTokenKind.True, "true", pos) } |
        str("false") ^^ { _ => GToken(GTokenKind.False, "false", pos) } |
        str("null") ^^ { _ => GToken(GTokenKind.Null, "null", pos) } |
        char('{') ^^ { _ => GToken(GTokenKind.LBrace, "{", pos) } |
        char('}') ^^ { _ => GToken(GTokenKind.RBrace, "}", pos) } |
        char('[') ^^ { _ => GToken(GTokenKind.LBracket, "[", pos) } |
        char(']') ^^ { _ => GToken(GTokenKind.RBracket, "]", pos) } |
        char(':') ^^ { _ => GToken(GTokenKind.Colon, ":", pos) } |
        char(',') ^^ { _ => GToken(GTokenKind.Comma, ",", pos) }

object GrammaJSONParser extends TokenParsers[GToken]:
  def tokenPos(token: GToken): Pos = token.pos

  private def tok(kind: GTokenKind, msg: String)(using ctx: ParseCtx): P[GToken] =
    accept(_.kind == kind, msg)

  def stringLit(using ctx: ParseCtx): P[String] =
    tok(GTokenKind.StringLit, "string") ^^ { _.text }

  def numberLit(using ctx: ParseCtx): P[Double] =
    tok(GTokenKind.NumberLit, "number") ^^ { _.text.toDouble }

  def value(using ctx: ParseCtx): P[JValue] =
    jObject | jArray | jString | jNumber | jBool | jNull

  def jString(using ctx: ParseCtx): P[JValue] =
    stringLit ^^ { s => JString(s) }

  def jNumber(using ctx: ParseCtx): P[JValue] =
    numberLit ^^ { n => JNumber(n) }

  def jBool(using ctx: ParseCtx): P[JValue] =
    tok(GTokenKind.True, "true") ^^ { _ => JBool(true) } |
      tok(GTokenKind.False, "false") ^^ { _ => JBool(false) }

  def jNull(using ctx: ParseCtx): P[JValue] =
    tok(GTokenKind.Null, "null") ^^ { _ => JNull }

  def jArray(using ctx: ParseCtx): P[JValue] =
    tok(GTokenKind.LBracket, "'['") ~> repsep(value, tok(GTokenKind.Comma, "','")) <~ tok(GTokenKind.RBracket, "']'") ^^ { elems =>
      JArray(elems)
    }

  def jObject(using ctx: ParseCtx): P[JValue] =
    tok(GTokenKind.LBrace, "'{'") ~> repsep(field, tok(GTokenKind.Comma, "','")) <~ tok(GTokenKind.RBrace, "'}'") ^^ { fields =>
      JObject(fields)
    }

  def field(using ctx: ParseCtx): P[(String, JValue)] =
    stringLit ~ (tok(GTokenKind.Colon, "':'") ~> value) ^^ {
      case key ~ v => (key, v)
    }

  def parseTokens(tokens: Array[GToken]): Either[ParseError, JValue] =
    parse(tokens, value)

object GrammaJSON:
  def parse(input: String): Either[ParseError, JValue] =
    for
      tokens <- GrammaJSONLexer.tokenize(input, GrammaJSONLexer.nextToken)
      ast    <- GrammaJSONParser.parseTokens(tokens)
    yield ast

  def parseLazy(input: String): Either[ParseError, JValue] =
    GrammaJSONParser.parseSource(input, GrammaJSONLexer, GrammaJSONLexer.nextToken)(GrammaJSONParser.value)
