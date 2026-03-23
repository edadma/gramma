# gramma

![Maven Central](https://img.shields.io/maven-central/v/io.github.edadma/gramma_sjs1_3)
[![Last Commit](https://img.shields.io/github/last-commit/edadma/gramma)](https://github.com/edadma/gramma/commits)
![GitHub](https://img.shields.io/github/license/edadma/gramma)
![Scala Version](https://img.shields.io/badge/Scala-3.8.2-blue.svg)
![ScalaJS Version](https://img.shields.io/badge/Scala.js-1.20.2-blue.svg)
![Scala Native Version](https://img.shields.io/badge/Scala_Native-0.5.10-blue.svg)

A general-purpose Scala 3 parsing library built around a single abstract base that supports both lexical analysis and syntax parsing. A lexer is just a parser over characters, and a syntax parser is a parser over tokens — both share the same combinator vocabulary and performance model.

## Goals

- **Performance** comparable to fastparse — parsers as methods, mutable context, `inline` combinators, no parser object graph, no result allocation
- **Readability** comparable to Scala's combinator library — `~`, `|`, `^^`, `~>`, `<~` operators, grammar rules that look like grammar rules
- **Rich error reporting** — every token carries source position from lex time; furthest failure heuristic points at the right token with full source context

## Installation

```scala
libraryDependencies += "io.github.edadma" %%% "gramma" % "0.0.1"
```

Cross-compiled for JVM, Scala.js, and Scala Native.

## Quick Example

### Define your tokens

```scala
import io.github.edadma.gramma.*

enum TokenKind:
  case Ident, IntLit, StringLit
  case Keyword(word: String)
  case Punct(ch: Char)

case class Token(kind: TokenKind, text: String, pos: Pos)
```

### Write a lexer

```scala
object MyLexer extends Lexers:
  val keywords = Set("if", "then", "else", "true", "false")

  def nextToken(using ctx: LexCtx): P[Token] =
    skipWhitespace
    val pos = ctx.capturePos()
    identifier(_.isLetter || _ == '_', c => c.isLetterOrDigit || c == '_') ^^ { text =>
      if keywords.contains(text) then Token(TokenKind.Keyword(text), text, pos)
      else Token(TokenKind.Ident, text, pos)
    } | integerLit ^^ { text => Token(TokenKind.IntLit, text, pos) }
      | stringLit('\'', '\\') ^^ { text => Token(TokenKind.StringLit, text, pos) }
      | charIn("(),;+-*/") ^^ { c => Token(TokenKind.Punct(c), c.toString, pos) }
```

### Write a parser

```scala
object MyParser extends TokenParsers[Token]:
  def tokenPos(token: Token): Pos = token.pos

  def kw(word: String)(using ctx: ParseCtx): P[Token] =
    accept(t => t.kind == TokenKind.Keyword(word), s"'$word'")

  def ident(using ctx: ParseCtx): P[String] =
    accept(_.kind == TokenKind.Ident, "identifier") ^^ { _.text }

  def intLit(using ctx: ParseCtx): P[Int] =
    accept(_.kind == TokenKind.IntLit, "integer") ^^ { _.text.toInt }

  def expr(using ctx: ParseCtx): P[Expr] =
    addExpr

  def addExpr(using ctx: ParseCtx): P[Expr] =
    leftAssoc(mulExpr, punct('+') | punct('-')) { (l, op, r) => BinaryExpr(l, op, r) }
```

### Run it

```scala
val input = "x + 1 * 2"

for
  tokens <- MyLexer.tokenize(input, MyLexer.nextToken)
  ast    <- MyParser.parse(tokens, MyParser.expr)
yield ast
```

## Architecture

```
String
  └─ LexCtx (ParseCtx[Char] + line/col tracking)
       └─ Array[Token]  (each Token carries Pos)
            └─ ParseCtx[Token]
                 └─ AST
```

Both phases are instances of the same abstract `Parsers[T]`. The library has no opinion about what a `Token` looks like — that is entirely user-defined.

## Combinators

All combinators are `inline def`s — they compile to direct code at the call site with no method objects, closures, or indirection.

| Combinator | Type | Description |
|---|---|---|
| `p ~ q` | `P[A ~ B]` | Sequence, returns pair for pattern matching |
| `p ~> q` | `P[B]` | Sequence, discard left |
| `p <~ q` | `P[A]` | Sequence, discard right |
| `p \| q` | `P[B]` | Alternation (committed choice) |
| `p ^^ f` | `P[B]` | Map result |
| `rep(p)` | `P[List[A]]` | Zero or more |
| `rep1(p)` | `P[List[A]]` | One or more |
| `repsep(p, sep)` | `P[List[A]]` | Zero or more with separator |
| `rep1sep(p, sep)` | `P[List[A]]` | One or more with separator |
| `opt(p)` | `P[Option[A]]` | Optional |
| `peek(p)` | `Boolean` | Lookahead without consuming |
| `not(p)` | `Unit` | Negative lookahead |
| `leftAssoc(p, op)(f)` | `P[A]` | Left-associative binary expressions |
| `positioned(p)` | `P[A]` | Stamp AST node with source position |

## Committed Choice

Alternation uses committed choice: if the left branch of `|` consumes any input before failing, the right branch is never tried. No explicit `cut` operator is needed — commitment happens automatically at the point of token consumption.

## Error Reporting

Errors use the **furthest failure** heuristic. The furthest point reached during parsing is almost always the location of the real error. Every token carries a `Pos` from lex time, so error messages include the full source line with a caret:

```
3:15: expected ')'
  foo(bar, baz
              ^
```

## Lexer Primitives

| Method | Description |
|---|---|
| `char(c)` | Match a specific character |
| `charIn(chars)` | Match any character in the string |
| `charWhere(pred, msg)` | Match character by predicate |
| `str(s)` | Match an exact string |
| `identifier(start, rest)` | Parameterised identifier rule |
| `digits` | One or more digits |
| `integerLit` | Integer literal |
| `decimalLit` | Decimal literal (with optional `.fraction`) |
| `stringLit(quote, escape)` | String literal with configurable quotes/escapes |
| `whitespace` | Skip whitespace characters |
| `lineComment(start)` | Skip line comment (e.g. `"//"`) |
| `blockComment(open, close, nested)` | Skip block comment, optionally nested |
| `skipWhitespace` | Combined whitespace + comment skipping |

## Building

```bash
sbt compile        # All platforms
sbt grammaJVM/compile   # JVM only
sbt grammaJS/compile    # Scala.js only
sbt grammaNative/compile # Scala Native only
```

## License

ISC License — see [LICENSE](LICENSE) for details.
