package io.github.edadma.gramma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class IntegrationTests extends AnyFreeSpec with Matchers:

  // --- Token / AST definitions for a small expression language ---

  enum TokenKind:
    case Ident, IntLit
    case Keyword(word: String)
    case Punct(ch: Char)
    case Op(sym: String)

  case class Token(kind: TokenKind, text: String, pos: Pos)

  sealed trait Expr extends Positional
  case class NumExpr(value: Int) extends Expr
  case class VarExpr(name: String) extends Expr
  case class BinaryExpr(left: Expr, op: String, right: Expr) extends Expr
  case class IfExpr(cond: Expr, thenE: Expr, elseE: Option[Expr]) extends Expr
  case class CallExpr(name: String, args: List[Expr]) extends Expr
  case class NegExpr(expr: Expr) extends Expr

  // --- Lexer ---

  object ExprLexer extends Lexers:
    val keywords = Set("if", "then", "else")

    def nextToken(using ctx: LexCtx): P[Token] =
      skipWhitespace("//")
      if ctx.atEnd then
        ctx.ok = false
        fail
      else
        val pos = ctx.capturePos()
        identifier(c => c.isLetter || c == '_', c => c.isLetterOrDigit || c == '_') ^^ { text =>
          if keywords.contains(text) then Token(TokenKind.Keyword(text), text, pos)
          else Token(TokenKind.Ident, text, pos)
        } | digits ^^ { text => Token(TokenKind.IntLit, text, pos) }
          | str("<=") ^^ { text => Token(TokenKind.Op(text), text, pos) }
          | str(">=") ^^ { text => Token(TokenKind.Op(text), text, pos) }
          | str("==") ^^ { text => Token(TokenKind.Op(text), text, pos) }
          | charIn("+-*/<>=") ^^ { c => Token(TokenKind.Op(c.toString), c.toString, pos) }
          | charIn("(),") ^^ { c => Token(TokenKind.Punct(c), c.toString, pos) }

  // --- Parser ---

  object ExprParser extends TokenParsers[Token]:
    def tokenPos(token: Token): Pos = token.pos

    def kw(word: String)(using ctx: ParseCtx): P[Token] =
      accept(t => t.kind == TokenKind.Keyword(word), s"'$word'")

    def ident(using ctx: ParseCtx): P[String] =
      accept(_.kind == TokenKind.Ident, "identifier") ^^ { _.text }

    def intLit(using ctx: ParseCtx): P[Int] =
      accept(_.kind == TokenKind.IntLit, "integer") ^^ { _.text.toInt }

    def punct(ch: Char)(using ctx: ParseCtx): P[Token] =
      accept(_.kind == TokenKind.Punct(ch), s"'$ch'")

    def op(sym: String)(using ctx: ParseCtx): P[String] =
      accept(t => t.kind == TokenKind.Op(sym), s"'$sym'") ^^ { _.text }

    def expr(using ctx: ParseCtx): P[Expr] =
      ifExpr | addExpr

    def ifExpr(using ctx: ParseCtx): P[Expr] =
      positioned {
        kw("if") ~> addExpr ~ (kw("then") ~> addExpr) ~ opt(kw("else") ~> addExpr) ^^ {
          case cond ~ thenE ~ elseE => IfExpr(cond, thenE, elseE)
        }
      }

    def addExpr(using ctx: ParseCtx): P[Expr] =
      leftAssoc(mulExpr, op("+") | op("-")) { (l, o, r) => BinaryExpr(l, o, r) }

    def mulExpr(using ctx: ParseCtx): P[Expr] =
      leftAssoc(unaryExpr, op("*") | op("/")) { (l, o, r) => BinaryExpr(l, o, r) }

    def unaryExpr(using ctx: ParseCtx): P[Expr] =
      (op("-") ~> atom ^^ { e => NegExpr(e) }) | atom

    def atom(using ctx: ParseCtx): P[Expr] =
      positioned {
        intLit ^^ { n => NumExpr(n) }
      } | positioned {
        ident ~ opt(punct('(') ~> repsep(expr, punct(',')) <~ punct(')')) ^^ {
          case name ~ Some(args) => CallExpr(name, args)
          case name ~ None       => VarExpr(name)
        }
      } | (punct('(') ~> expr <~ punct(')'))

    def parseTokens(tokens: Array[Token]): Either[ParseError, Expr] =
      parse(tokens, expr)

  // --- Helper ---

  def parseExpr(input: String): Either[ParseError, Expr] =
    for
      tokens <- ExprLexer.tokenize(input, ExprLexer.nextToken)
      ast    <- ExprParser.parseTokens(tokens)
    yield ast

  // --- Happy path tests ---

  "parse integer literal" in {
    parseExpr("42") shouldBe Right(NumExpr(42))
  }

  "parse variable" in {
    parseExpr("x") shouldBe Right(VarExpr("x"))
  }

  "parse addition" in {
    parseExpr("1 + 2") shouldBe Right(BinaryExpr(NumExpr(1), "+", NumExpr(2)))
  }

  "parse left-associative chain" in {
    parseExpr("1 + 2 + 3") shouldBe Right(
      BinaryExpr(BinaryExpr(NumExpr(1), "+", NumExpr(2)), "+", NumExpr(3))
    )
  }

  "parse precedence: mul binds tighter than add" in {
    parseExpr("1 + 2 * 3") shouldBe Right(
      BinaryExpr(NumExpr(1), "+", BinaryExpr(NumExpr(2), "*", NumExpr(3)))
    )
  }

  "parse parenthesized expression" in {
    parseExpr("(1 + 2) * 3") shouldBe Right(
      BinaryExpr(BinaryExpr(NumExpr(1), "+", NumExpr(2)), "*", NumExpr(3))
    )
  }

  "parse negation" in {
    parseExpr("- 1") shouldBe Right(NegExpr(NumExpr(1)))
  }

  "parse function call no args" in {
    parseExpr("f()") shouldBe Right(CallExpr("f", Nil))
  }

  "parse function call with args" in {
    parseExpr("add(1, 2)") shouldBe Right(CallExpr("add", List(NumExpr(1), NumExpr(2))))
  }

  "parse if-then" in {
    parseExpr("if 1 then 2") shouldBe Right(IfExpr(NumExpr(1), NumExpr(2), None))
  }

  "parse if-then-else" in {
    parseExpr("if 1 then 2 else 3") shouldBe Right(IfExpr(NumExpr(1), NumExpr(2), Some(NumExpr(3))))
  }

  "parse complex expression" in {
    parseExpr("if x + 1 then f(y, 2 * z) else 0") shouldBe Right(
      IfExpr(
        BinaryExpr(VarExpr("x"), "+", NumExpr(1)),
        CallExpr("f", List(VarExpr("y"), BinaryExpr(NumExpr(2), "*", VarExpr("z")))),
        Some(NumExpr(0)),
      )
    )
  }

  // --- Comments ---

  "comments are skipped" in {
    parseExpr("1 + // comment\n2") shouldBe Right(BinaryExpr(NumExpr(1), "+", NumExpr(2)))
  }

  // --- Positioned ---

  "positioned stamps AST node with source location" in {
    val result = parseExpr("42")
    result match
      case Right(n: NumExpr) =>
        n.pos.line shouldBe 1
        n.pos.col shouldBe 1
      case other => fail(s"unexpected: $other")
  }

  "positioned reports correct column for later token" in {
    val result = parseExpr("1 + 42")
    result match
      case Right(BinaryExpr(_, _, r: NumExpr)) =>
        r.pos.col shouldBe 5
      case other => fail(s"unexpected: $other")
  }

  // --- Error reporting ---

  "error on unexpected token" in {
    val result = parseExpr("1 +")
    result match
      case Left(err) =>
        err.pos.line shouldBe 1
        err.msg should (include("integer") or include("identifier") or include("'-'") or include("'('"))
      case Right(_) => fail("expected parse error")
  }

  "error on unclosed parenthesis" in {
    val result = parseExpr("(1 + 2")
    result match
      case Left(err) =>
        err.msg should include("')'")
      case Right(_) => fail("expected parse error")
  }

  "error includes source line text" in {
    val result = parseExpr("1 + + 2")
    result match
      case Left(err) =>
        err.pos.lineText shouldBe "1 + + 2"
      case Right(_) => fail("expected parse error")
  }

  "error caret points at correct column" in {
    val result = parseExpr("1 + + 2")
    result match
      case Left(err) =>
        val msg = err.toString
        // Should have a caret line
        val lines = msg.split('\n')
        lines.length shouldBe 3 // "line:col: msg", source line, caret
        lines(2).trim shouldBe "^"
      case Right(_) => fail("expected parse error")
  }

  "multiline error reports correct line" in {
    val input = "1 +\n2 *\n+ 3"
    val result = parseExpr(input)
    result match
      case Left(err) =>
        err.pos.line shouldBe 3
        err.pos.col shouldBe 1
        err.pos.lineText shouldBe "+ 3"
      case Right(_) => fail("expected parse error")
  }

  "lexer error on invalid character" in {
    val result = parseExpr("1 + @")
    result shouldBe a[Left[?, ?]]
  }
