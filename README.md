# gramma

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/gramma_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/gramma)](https://github.com/edadma/gramma/commits)
![GitHub](https://img.shields.io/github/license/edadma/gramma)
![Scala Version](https://img.shields.io/badge/Scala-3.8.2-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.20.2-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.10-blue.svg)

A Scala 3 parsing library with the ergonomics of scala-parser-combinators and performance close to fastparse. Supports separate lexing and parsing phases with automatic tokenization, and optional indentation-sensitive parsing.

## Quick Start

```scala
import io.github.edadma.gramma.*

// 1. Define your lexer — just declare keywords and delimiters
object MyLexer extends StdLexer:
  delimiters ++= List("(", ")", "+", "-", "*", "/", ",")
  reserved ++= List("if", "then", "else")

// 2. Define your parser — ident, stringLit, numericLit are built in.
//    Bare strings automatically match keywords or delimiters.
object MyParser extends StdParsers(MyLexer):
  import scala.language.implicitConversions

  def expr(using ctx: ParseCtx): P[Int] =
    numericLit ^^ (_.toInt) |
      "if" ~> expr ~ ("then" ~> expr) ~ ("else" ~> expr) ^^ {
        case cond ~ t ~ e => if cond != 0 then t else e
      }

// 3. Parse
MyParser.parseSource("if 1 then 42 else 0")(MyParser.expr)
// Right(42)
```

No token types to define, no lexer rules to write. Identifiers, strings, numbers, keywords, and delimiters are handled automatically. Bare strings in parser rules match keywords or delimiters implicitly.

## Installation

```scala
libraryDependencies += "io.github.edadma" %%% "gramma" % "0.0.1"
```

Cross-compiled for JVM, Scala.js, and Scala Native.

## JSON Parser Example

A complete JSON parser in ~25 lines:

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
  import scala.language.implicitConversions

  def value(using ctx: ParseCtx): P[JValue] =
    obj | arr | stringLit ^^ (JString(_)) |
      numericLit ^^ (n => JNumber(n.toDouble)) |
      "true" ^^ (_ => JBool(true)) |
      "false" ^^ (_ => JBool(false)) |
      "null" ^^ (_ => JNull)

  def arr(using ctx: ParseCtx): P[JValue] =
    "[" ~> repsep(value, ",") <~ "]" ^^ (JArray(_))

  def obj(using ctx: ParseCtx): P[JValue] =
    "{" ~> repsep(field, ",") <~ "}" ^^ (JObject(_))

  def field(using ctx: ParseCtx): P[(String, JValue)] =
    stringLit ~ (":" ~> value) ^^ { case k ~ v => (k, v) }

// Parse
JSONParser.parseSource("""{"name": "Alice", "age": 30}""")(JSONParser.value)
```

## Indentation-Sensitive Parsing

Gramma supports Python-style indentation parsing. Enable it on your lexer and the library automatically emits `Indent`, `Dedent`, and `Newline` tokens:

```scala
object YAMLLexer extends StdLexer:
  override protected def indentSensitive: Boolean = true
  delimiters ++= List(":", "-")
  reserved ++= List("true", "false", "null")

object YAMLParser extends StdParsers(YAMLLexer):
  import scala.language.implicitConversions

  def document(using ctx: ParseCtx): P[YValue] =
    blockMap | blockList | scalar

  def blockMap(using ctx: ParseCtx): P[YValue] =
    rep1sep(mapEntry, newline) ^^ (entries => YMap(entries))

  def mapEntry(using ctx: ParseCtx): P[(String, YValue)] =
    ident ~ (":" ~> mapValue) ^^ { case k ~ v => (k, v) }

  def mapValue(using ctx: ParseCtx): P[YValue] =
    scalar | block(value)  // inline value or indented block

  def blockList(using ctx: ParseCtx): P[YValue] =
    rep1sep("-" ~> value, newline) ^^ (items => YList(items))
```

This parses:

```yaml
name: Alice
address:
  city: Springfield
  zip: 12345
hobbies:
  - reading
  - coding
```

Indentation rules follow Python semantics:
- Brackets (`()`, `[]`, `{}`) suppress indentation — newlines inside brackets are ignored
- Blank lines and comment-only lines are ignored
- Inconsistent dedentation is an error

## Performance

Benchmarked against [fastparse](https://com-lihaoyi.github.io/fastparse/) and [scala-parser-combinators](https://github.com/scala/scala-parser-combinators) parsing JSON on all three platforms. Times in microseconds (lower is better):

### JVM

| Input | gramma | fastparse | scala-combinators |
|---|---|---|---|
| **Small** (44 chars) | 1.6 µs | 0.4 µs | 25 µs |
| **Medium** (1.6K chars) | 13 µs | 9 µs | 671 µs |
| **Large** (11K chars) | 96 µs | 72 µs | 72 µs |

### Scala Native (releaseFast)

| Input | gramma | fastparse | scala-combinators |
|---|---|---|---|
| **Small** (44 chars) | 2.4 µs | 0.7 µs | 65 µs |
| **Medium** (1.6K chars) | 61 µs | 18 µs | 1,650 µs |
| **Large** (11K chars) | 463 µs | 148 µs | 170 µs |

### Scala.js (Node/V8)

| Input | gramma | fastparse | scala-combinators |
|---|---|---|---|
| **Small** (44 chars) | 3.0 µs | 1.3 µs | 54 µs |
| **Medium** (1.6K chars) | 83 µs | 31 µs | 1,498 µs |
| **Large** (11K chars) | 627 µs | 278 µs | 206 µs |

On small/medium inputs (typical source files), gramma is **15-50x faster** than scala-combinators across all platforms. Fastparse is faster due to macro-generated code. Gramma's design focuses on separate lexing/parsing phases with proper token types and source positions.

A 1.6K source file parses in **13 µs on JVM**, **61 µs on Native**, **83 µs on JS**.

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

| Override | Default | Description |
|---|---|---|
| `isIdentStart(c)` | letter or `_` | First character of identifiers |
| `isIdentPart(c)` | letter, digit, or `_` | Subsequent identifier characters |
| `stringQuote` | `'"'` | String literal quote character |
| `stringEscape` | `'\\'` | String literal escape character |
| `skip` | whitespace | Whitespace/comment skipping |
| `indentSensitive` | `false` | Enable INDENT/DEDENT tokens |
| `customToken` | `None` | Hook for custom token types |

## StdParsers

`StdParsers` pairs with a `StdLexer` and provides built-in token matchers:

| Method | Returns | Description |
|---|---|---|
| `ident` | `P[String]` | Match identifier, return text |
| `stringLit` | `P[String]` | Match string literal, return content |
| `numericLit` | `P[String]` | Match numeric literal, return text |
| `keyword(word)` | `P[String]` | Match specific keyword |
| `delimiter(d)` | `P[String]` | Match specific delimiter |
| `indent` | `P[Unit]` | Match indent token |
| `dedent` | `P[Unit]` | Match dedent token |
| `newline` | `P[Unit]` | Match newline token (same indent level) |
| `block(p)` | `P[A]` | Match `indent ~> p <~ dedent` |

Bare strings are implicitly converted to keyword or delimiter matchers (requires `import scala.language.implicitConversions`):

```scala
// These are equivalent:
keyword("if") ~> expr ~ (keyword("then") ~> expr)
"if" ~> expr ~ ("then" ~> expr)
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
case class Token(kind: TokenKind, text: String, pos: Pos)

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
sbt grammaJVM/run        # Run cross-platform benchmarks
```

## License

ISC License — see [LICENSE](LICENSE) for details.
