# Gramma — Design Document

## Name

**gramma** — from Greek *gramma* (γράμμα), a written character or symbol. The library deals
with symbols at every level: characters in the lexer, tokens in the parser. *Grammar* derives
directly from the same root, which is exactly what users of this library are building.

- Maven / sbt: `"io.github.edadma" %%% "gramma"`
- Import: `import io.github.edadma.gramma.*`

## Overview

A general-purpose Scala 3 parsing library built around a single abstract base that supports
both lexical analysis and syntax parsing. The core idea is that a lexer is just a parser over
a stream of `Char` tokens, and a syntax parser is a parser over a stream of rich `Token` values
— both sharing the same combinator vocabulary and performance model.

The library targets three goals:

- **Performance** comparable to fastparse — parsers as methods, mutable context, `inline`
  combinators, no parser object graph, no result object allocation
- **Readability** comparable to Scala's combinator library — `~`, `|`, `^^`, `~>`, `<~`
  operators, grammar rules that look like grammar rules
- **Rich error reporting** — every token carries source position from lex time; the furthest
  failure heuristic points at the right token with full source line context


## Architecture

```
String
  └─ LexCtx (ParseCtx[Char] + line/col tracking)
       └─ Array[Token]  (each Token carries Pos)
            └─ ParseCtx[Token]
                 └─ AST
```

Both phases are instances of the same abstract `Parsers[T]`. The library has no opinion about
what a `Token` looks like — that is entirely user-defined.


## Core Abstractions

### `Parsers[T]`

The abstract base class, generic over token type `T`. Both `Lexers` and `TokenParsers` extend
this. Contains the `ParseCtx`, the `P[A]` type, all combinators, and all operators.

```scala
abstract class Parsers[T]:
  class ParseCtx(val tokens: Array[T]):
    var index: Int = 0
    var ok: Boolean = true
    var failAt: Int = 0
    var failMsg: String = ""

  opaque type P[A] = A
```

`ParseCtx` is a single mutable object per parse. No result objects are allocated for success or
failure — `ok` is a boolean flag, and successful results are plain return values.

`P[A]` is an opaque type alias for `A`. At runtime there is no wrapper — `P[Expr]` is just
`Expr`. The opaque type exists to allow extension methods to be defined on parser results in a
way that integrates naturally with the combinator syntax.

### `accept`

The single abstract primitive. All combinators are built on it.

```scala
def accept(pred: T => Boolean, msg: String)(using ctx: ParseCtx): P[T]
```

Checks the current token against `pred`. On success, advances the index and returns the token.
On failure, updates `failAt`/`failMsg` if this is the furthest failure seen so far, sets
`ctx.ok = false`, and returns a null/default value. The caller must check `ctx.ok` before using
the result — the combinator infrastructure handles this automatically.


## Combinator Set

All combinators are `inline def`s. They compile down to direct code at the call site — no
method objects, no closures, no indirection.

### Sequencing and Alternation

```scala
extension [A](p: P[A])
  inline def ~[B](inline q: => P[B])(using ctx: ParseCtx): P[A ~ B]
  inline def ~>[B](inline q: => P[B])(using ctx: ParseCtx): P[B]   // discard left
  inline def <~[B](inline q: => P[B])(using ctx: ParseCtx): P[A]   // discard right
  inline def |[B >: A](inline q: => P[B])(using ctx: ParseCtx): P[B]
  inline def ^^[B](inline f: A => B)(using ctx: ParseCtx): P[B]
```

`~` produces a `~` pair for pattern matching in `^^`:

```scala
case class ~[+A, +B](a: A, b: B)
```

### Repetition

```scala
inline def rep[A](inline p: => P[A])(using ctx: ParseCtx): P[List[A]]
inline def rep1[A](inline p: => P[A])(using ctx: ParseCtx): P[List[A]]
inline def repsep[A](inline p: => P[A], inline sep: => P[?])(using ctx: ParseCtx): P[List[A]]
inline def rep1sep[A](inline p: => P[A], inline sep: => P[?])(using ctx: ParseCtx): P[List[A]]
```

`rep1` and `rep1sep` fail if they match fewer than one element. `repsep` and `rep1sep` commit
after the separator is consumed — if the separator matches but the following `p` fails, that is
a hard error, not a soft backtrack.

### Optional

```scala
inline def opt[A](inline p: => P[A])(using ctx: ParseCtx): P[Option[A]]
```

### Lookahead

```scala
inline def peek[A](inline p: => P[A])(using ctx: ParseCtx): Boolean
inline def not[A](inline p: => P[A])(using ctx: ParseCtx): Unit
```

`peek` runs `p` without consuming input and returns whether it succeeded. `not` fails if `p`
succeeds — used for negative lookahead (e.g. keyword boundary checks in the lexer).

### Left Recursion and Expression Parsing

Recursive descent parsers cannot handle left-recursive rules — a rule that calls itself before
consuming any input loops infinitely. This affects gramma as it does every recursive descent
library. Left-recursive grammar rules must be rewritten iteratively.

The standard solution is a `rep`-then-`foldLeft` pattern. Given a left-associative binary
expression grammar:

```
expr ::= expr op term | term
```

The iterative rewrite is: parse the first operand, then repeatedly parse `(operator, operand)`
pairs and fold left over them to build the tree. Gramma provides this as a first-class
combinator since virtually every language parser needs it at every precedence level:

```scala
inline def leftAssoc[A](inline p: => P[A], inline op: => P[String])(f: (A, String, A) => A)(using ctx: ParseCtx): P[A] =
  val first = p
  val rest = rep(op ~ p)
  rest.foldLeft(first) { case (l, o ~ r) => f(l, o, r) }
```

Users express each precedence level cleanly without any boilerplate:

```scala
def addExpr(using ctx: ParseCtx[Token]): P[Expr] =
  leftAssoc(mulExpr, "+" | "-") { (l, op, r) => BinaryExpr(l, op, r) }

def mulExpr(using ctx: ParseCtx[Token]): P[Expr] =
  leftAssoc(unaryExpr, "*" | "/" | "%") { (l, op, r) => BinaryExpr(l, op, r) }
```

Right-associative operators (exponentiation, assignment) use the same shape but with `rep1sep`
and `foldRight` — not provided as a built-in since they are much less common, but trivial to
write in user code following the same pattern.

### Entry Point

```scala
def parse[A](tokens: Array[T], rule: ParseCtx ?=> P[A]): Either[ParseError, A]
```

Runs `rule` against `tokens`. Checks that all input was consumed after the rule succeeds. Returns
`Right(result)` or `Left(ParseError)` containing the formatted error message. This is the only
public API that is not `inline` — it is the boundary between mutable internals and a pure interface.


## Committed Choice

Alternation uses committed choice: if the left branch of `|` consumes any input before failing,
the right branch is never tried. The check is:

```scala
val savedIndex = ctx.index
val result = leftBranch
if ctx.ok then result
else if ctx.index > savedIndex then result  // consumed input — committed
else
  ctx.ok = true
  rightBranch
```

This matches Scala combinator library semantics. No explicit cut operator is needed — commitment
happens automatically at the point of token consumption. This is the right model for language
parsing because alternatives are almost always distinguished by their first token.


## Error Reporting

Errors are reported using the **furthest failure** heuristic. Whenever `accept` fails, if
`ctx.index >= ctx.failAt` the library updates `failAt` and `failMsg`. The furthest point reached
during parsing is almost always the location of the real error.

`ParseError` carries the full context needed for a useful message:

```scala
case class ParseError(pos: Pos, msg: String):
  override def toString: String =
    s"${pos.line}:${pos.col}: $msg\n${pos.lineText}\n${" " * (pos.col - 1)}^"
```

Since every `Token` carries a `Pos` from lex time, the error reporter just looks up
`tokens(failAt).pos` — no reconstruction needed.


## Position

```scala
case class Pos(line: Int, col: Int, lineText: String)
```

`lineText` is the full source line the token appears on, captured at lex time by scanning
backward to the previous `\n` and forward to the next. This is done once per token during
lexing — the cost is paid at lex time and is free at error reporting time.

### `Positional`

AST nodes that need source location mix in `Positional`:

```scala
trait Positional:
  var pos: Pos = Pos(0, 0, "")
  def setPos(p: Pos): this.type =
    pos = p
    this
```

The `positioned` combinator stamps the result with the position of the first token consumed:

```scala
inline def positioned[A <: Positional](inline p: => P[A])(using ctx: ParseCtx): P[A]
```


## Lexer Layer

`Lexers` extends `Parsers[Char]` and adds a specialised context for character-level parsing:

```scala
abstract class Lexers extends Parsers[Char]:

  class LexCtx(val source: String) extends ParseCtx(source.toCharArray):
    var line: Int = 1
    var col: Int = 1
    private var lineStart: Int = 0

    override def advance(): Unit =
      if tokens(index) == '\n' then
        line += 1
        col = 1
        lineStart = index + 1
      else
        col += 1
      super.advance()

    def capturePos(): Pos =
      val lineEnd = source.indexOf('\n', index) match
        case -1 => source.length
        case n  => n
      Pos(line, col, source.substring(lineStart, lineEnd))

    def tokenText(start: Int): String =
      source.substring(start, index)
```

`LexCtx` is the only place line/col state lives. Position is captured at the start of each
token rule via `capturePos()`, and the token text is extracted via `tokenText(start)` after
the rule has consumed all its characters. Both are cheap operations.

### Lexer Helpers

Standard character-matching primitives provided by `Lexers`:

```scala
def char(c: Char)(using ctx: LexCtx): P[Char]
def charIn(chars: String)(using ctx: LexCtx): P[Char]
def charWhere(pred: Char => Boolean, msg: String)(using ctx: LexCtx): P[Char]
def str(s: String)(using ctx: LexCtx): P[String]
```

Everything else — identifier rules, number literal rules, string literal rules — is written by
the user in terms of these primitives and the shared combinators.

### Lexer Convenience Methods

`Lexers` provides reusable lexical patterns as methods on `LexCtx`. Every lexer needs these
and they should not be reimplemented each time. All return the matched text as a `String` so
the user can wrap it in whatever token type they define.

**Identifiers** — parameterised so users can define their own rules (SQL, Java, Lisp all differ):

```scala
def identifier(start: Char => Boolean, rest: Char => Boolean)(using ctx: LexCtx): P[String]
```

**Number literals**:

```scala
def digits(using ctx: LexCtx): P[String]
def integerLit(using ctx: LexCtx): P[String]
def decimalLit(using ctx: LexCtx): P[String]
```

**String literals** — configurable quote character and escape character, covering both
`'single quoted'` and `"double quoted"` styles:

```scala
def stringLit(quote: Char, escape: Char)(using ctx: LexCtx): P[String]
```

**Whitespace and comments**:

```scala
def whitespace(using ctx: LexCtx): P[Unit]
def lineComment(start: String)(using ctx: LexCtx): P[Unit]
def blockComment(open: String, close: String, nested: Boolean)(using ctx: LexCtx): P[Unit]
def skipWhitespace(using ctx: LexCtx): P[Unit]
```

`skipWhitespace` combines whitespace, line comments, and block comments in a single call — the
typical lexer preamble before each token. The `nested` flag on `blockComment` handles SQL-style
`/* /* */ */` nesting directly.

With these in place a user's lexer rule becomes a thin wrapper that classifies matched text into
token kinds rather than reimplementing character-level machinery:

```scala
object MyLexer extends Lexers:
  def nextToken(using ctx: LexCtx): P[Token] =
    skipWhitespace
    val pos = ctx.capturePos()
    identifier(_.isLetter, _.isLetterOrDigit) ^^ { text =>
      if keywords.contains(text) then Token(Keyword(text), text, pos)
      else Token(Ident, text, pos)
    } | decimalLit ^^ { text => Token(DecimalLit, text, pos) }
      | integerLit ^^ { text => Token(IntLit, text, pos) }
      | stringLit('\'', '\'') ^^ { text => Token(StringLit, text, pos) }
      | charIn("+-*/<>=!") ^^ { text => Token(Op, text, pos) }
      | charIn("(),;") ^^ { text => Token(Punct, text, pos) }
```

### Running the Lexer

The lexer entry point converts a `String` to an `Array[Token]`:

```scala
def tokenize(source: String, rule: LexCtx ?=> P[Token]): Either[ParseError, Array[Token]]
```

The result feeds directly into the syntax parser.


## Syntax Parser Layer

`TokenParsers[Token]` extends `Parsers[Token]`. The token type is user-defined — the library
has no built-in concept of keyword, identifier, or operator.

```scala
abstract class TokenParsers[Token] extends Parsers[Token]
```

Users implement `accept` in terms of their own token type and provide their own matching
primitives. A typical implementation:

```scala
object MyParser extends TokenParsers[MyToken]:

  def token(kind: MyTokenKind, msg: String)(using ctx: ParseCtx): P[MyToken] =
    accept(_.kind == kind, msg)

  def kw(word: String)(using ctx: ParseCtx): P[MyToken] =
    accept(t => t.kind == Keyword && t.text == word, s"keyword '$word'")

  def ident(using ctx: ParseCtx): P[MyToken] =
    accept(_.kind == Ident, "identifier")
```


## Cross-Platform

The library is a Scala 3 cross-project targeting JVM, JS, and Native — the same targets as
PetraDB. It has no platform-specific dependencies. `Array[T]`, mutable vars, and `String`
operations are all available on all three platforms.


## What the Library Does Not Do

- **Error recovery** — one error is reported and parsing stops. Recovery is grammar-specific
  and out of scope.
- **Incremental parsing** — the full source is loaded as a `String` before lexing begins.
  Source files are small enough that streaming provides no practical benefit and would
  complicate both the API and the implementation.
- **Define token types** — token representation is entirely user-defined. The library only
  requires that tokens can be matched by a predicate.
- **Define lexer rules** — the library provides character primitives; the user writes the rules.


## Usage Sketch

```scala
// ── Token definition (user code) ─────────────────────────

enum TokenKind:
  case Ident, IntLit, StringLit
  case Keyword(word: String)
  case Punct(ch: Char)
  case EOF

case class Token(kind: TokenKind, text: String, pos: Pos)

// ── Lexer (user code) ────────────────────────────────────

object MyLexer extends Lexers:
  val keywords = Set("if", "then", "else", "true", "false")

  def nextToken(using ctx: LexCtx): P[Token] =
    skipWhitespace
    val pos = ctx.capturePos()
    identifier(_.isLetter || _ == '_', c => c.isLetterOrDigit || c == '_') ^^ { text =>
      if keywords.contains(text) then Token(TokenKind.Keyword(text), text, pos)
      else Token(TokenKind.Ident, text, pos)
    } | integerLit ^^ { text => Token(TokenKind.IntLit, text, pos) }
      | stringLit('\'', '\'') ^^ { text => Token(TokenKind.StringLit, text, pos) }
      | charIn("(),;") ^^ { text => Token(TokenKind.Punct(text.head), text, pos) }

// ── Parser (user code) ───────────────────────────────────

object MyParser extends TokenParsers[Token]:
  def kw(word: String)(using ctx: ParseCtx): P[Token] =
    accept(t => t.kind == TokenKind.Keyword(word), s"'$word'")

  def ifExpr(using ctx: ParseCtx): P[Expr] =
    positioned {
      kw("if") ~> expr ~ (kw("then") ~> expr) ~ opt(kw("else") ~> expr) ^^ {
        case cond ~ thenE ~ elseE => IfExpr(cond, thenE, elseE)
      }
    }

  def expr(using ctx: ParseCtx): P[Expr] =
    ifExpr | addExpr

  def statement(using ctx: ParseCtx): P[Statement] =
    createTable | insert | update | delete | selectStmt
```


## Key Design Decisions Summary

| Decision | Choice | Rationale |
|---|---|---|
| Combinator model | Methods + mutable context | No parser object allocation, `inline` eliminates call overhead |
| `P[A]` | Opaque type alias for `A` | Enables operator syntax with zero runtime overhead |
| Alternation semantics | Committed choice on token consumption | Natural for language grammars, no explicit cut needed |
| Error strategy | Furthest failure | Simple, effective, points at the right location |
| Position capture | At lex time, embedded in tokens | Free at parse/error time, one cost per token |
| Lexer/parser unification | Single abstract base `Parsers[T]` | Same combinators, same operators, same error model for both phases |
| Token type | User-defined | Library is truly general, no SQL or language-specific concepts |
| Input model | Full `String` loaded upfront | Source files are small; streaming adds complexity with no benefit |
| Error recovery | Not supported | Grammar-specific, out of scope for a general library |
