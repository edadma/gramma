# gramma

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/gramma_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/gramma)](https://github.com/edadma/gramma/commits)
![GitHub](https://img.shields.io/github/license/edadma/gramma)
![Scala Version](https://img.shields.io/badge/Scala-3.8.2-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.20.2-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.10-blue.svg)

A Scala 3 parsing library with the ergonomics of scala-parser-combinators and performance close to fastparse. Supports separate lexing and parsing phases with automatic tokenization.

## Quick Start

```scala
import io.github.edadma.gramma.*

// 1. Define your lexer — just declare keywords and delimiters
object MyLexer extends StdLexer:
  delimiters ++= List("(", ")", "+", "-", "*", "/", ",")
  reserved ++= List("if", "then", "else")

// 2. Define your parser — ident, stringLit, numericLit, keyword, delimiter are built in
object MyParser extends StdParsers(MyLexer):
  def expr(using ctx: ParseCtx): P[Int] =
    numericLit ^^ (_.toInt) |
      keyword("if") ~> expr ~ (keyword("then") ~> expr) ~ (keyword("else") ~> expr) ^^ {
        case cond ~ t ~ e => if cond != 0 then t else e
      }

// 3. Parse
MyParser.parseSource("if 1 then 42 else 0")(MyParser.expr)
// Right(42)
```

No token types to define, no lexer rules to write. Identifiers, strings, numbers, keywords, and delimiters are handled automatically.

## Installation

```scala
libraryDependencies += "io.github.edadma" %%% "gramma" % "0.0.1"
```

Cross-compiled for JVM, Scala.js, and Scala Native.

## JSON Parser Example

A complete JSON parser in ~30 lines:

```scala
import io.github.edadma.gramma.*

sealed trait JValue
case class JObject(fields: List[(String, JValue)]) extends JValue
case class JArray(elements: List[JValue]) extends JValue
case class JString(value: String) extends JValue
case class JNumber(value: Double) extends JValue
case class JBool(value: Boolean) extends JValue
case object JNull extends JValue

object JSONLexer extends StdLexer:
  delimiters ++= List("{", "}", "[", "]", ":", ",")
  reserved ++= List("true", "false", "null")

object JSONParser extends StdParsers(JSONLexer):
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

// Parse
JSONParser.parseSource("""{"name": "Alice", "age": 30}""")(JSONParser.value)
```

## Performance

Benchmarked against fastparse and scala-parser-combinators parsing JSON (ops/s, higher is better):

| Input | gramma | fastparse | scala-combinators |
|---|---|---|---|
| **Small** (44 chars) | 1,984K | 3,518K | 40K |
| **Medium** (1.6K chars) | 83K | 114K | 1.5K |
| **Large** (10.6K chars) | 11K | 16K | 14K |

Gramma is within 60-75% of fastparse and 50x faster than scala-combinators on typical inputs. A 10K source file parses in ~90 microseconds.

## Combinators

| Combinator | Type | Description |
|---|---|---|
| `p ~ q` | `P[A ~ B]` | Sequence, returns pair for pattern matching |
| `p ~> q` | `P[B]` | Sequence, discard left |
| `p <~ q` | `P[A]` | Sequence, discard right |
| `p \| q` | `P[B]` | Alternation (committed choice) |
| `p ^^ f` | `P[B]` | Map result |
| `rep(p)` | `P[List[A]]` | Zero or more |
| `rep1(p)` | `P[List[A]]` | One or more |
| `repN(n, p)` | `P[List[A]]` | Exactly N repetitions |
| `repsep(p, sep)` | `P[List[A]]` | Zero or more with separator |
| `rep1sep(p, sep)` | `P[List[A]]` | One or more with separator |
| `opt(p)` | `P[Option[A]]` | Optional |
| `peek(p)` | `Boolean` | Lookahead without consuming |
| `not(p)` | `Unit` | Negative lookahead |
| `leftAssoc(p, op)(f)` | `P[A]` | Left-associative binary expressions |
| `positioned(p)` | `P[A]` | Stamp AST node with source position |
| `log(p, name)` | `P[A]` | Debug tracing (prints entry/exit) |

## StdLexer

`StdLexer` provides automatic tokenization. Declare your keywords and delimiters; identifiers, strings, and numbers are recognized automatically.

```scala
object MyLexer extends StdLexer:
  delimiters ++= List("<=", ">=", "==", "!=", "<", ">", "=", "+", "-", "*", "/")
  reserved ++= List("if", "then", "else", "true", "false")
```

Multi-character delimiters are matched longest-first (`<=` before `<`).

### Customization

Override methods to customize lexer behavior:

```scala
object MyLexer extends StdLexer:
  delimiters ++= List("+", "-")
  reserved ++= List("let", "in")

  // Single-quoted strings instead of double-quoted
  override protected def stringQuote: Char = '\''

  // Support line comments
  override protected def skip(using ctx: LexCtx): Unit =
    skipWhitespace("//")

  // Add custom token types (e.g., regex literals, heredocs)
  override protected def customToken(using ctx: LexCtx): Option[P[StdToken]] =
    if ctx.tokens(ctx.index) == '#' then
      // ... custom recognition logic
      Some(succeed(StdToken(StdTokenKind.Delimiter, "#", ctx.capturePos())))
    else None
```

## StdParsers

`StdParsers` pairs with a `StdLexer` and provides built-in token matchers:

| Method | Returns | Description |
|---|---|---|
| `ident` | `P[String]` | Match identifier, return text |
| `stringLit` | `P[String]` | Match string literal, return content |
| `numericLit` | `P[String]` | Match numeric literal, return text |
| `keyword(word)` | `P[String]` | Match specific keyword |
| `delimiter(d)` | `P[String]` | Match specific delimiter |

```scala
object MyParser extends StdParsers(MyLexer):
  def letExpr(using ctx: ParseCtx): P[Expr] =
    keyword("let") ~> ident ~ (delimiter("=") ~> expr) ~ (keyword("in") ~> expr) ^^ {
      case name ~ value ~ body => LetExpr(name, value, body)
    }
```

## Committed Choice

Alternation uses committed choice: if the left branch of `|` consumes any input before failing, the right branch is never tried. No explicit `cut` operator is needed — commitment happens automatically at the point of token consumption.

## Error Reporting

Errors use the **furthest failure** heuristic. Every token carries a `Pos` from lex time, so error messages include the full source line with a caret:

```
3:15: expected ')'
  foo(bar, baz
              ^
```

## Advanced: Custom Lexers

For languages that need non-standard tokenization, use `Lexers` and `TokenParsers` directly:

```scala
// Custom token type
case class Token(kind: TokenKind, text: String, pos: Pos)

// Custom lexer with full control
object MyLexer extends Lexers:
  def nextToken(using ctx: LexCtx): P[Token] =
    skipWhitespace("//", "/*", "*/", false)
    val pos = ctx.capturePos()
    firstChar {
      case c if c.isLetter => identifier(_.isLetter, _.isLetterOrDigit) ^^ { text => ... }
      case c if c.isDigit  => decimalLit ^^ { text => ... }
      case '"'             => stringLit('"', '\\') ^^ { text => ... }
      case _               => charIn("+-*/") ^^ { c => ... }
    }

// Custom parser
object MyParser extends TokenParsers[Token]:
  def tokenPos(token: Token): Pos = token.pos
  // ... define accept-based matchers for your token type
```

## Building

```bash
sbt compile              # All platforms
sbt grammaJVM/compile    # JVM only
sbt grammaJS/compile     # Scala.js only
sbt grammaNative/compile # Scala Native only
sbt grammaJVM/test       # Run tests
```

## License

ISC License — see [LICENSE](LICENSE) for details.
