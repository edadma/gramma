package io.github.edadma.gramma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LexerTests extends AnyFreeSpec with Matchers:

  object L extends Lexers:
    def parseChars[A](input: String)(rule: ParseCtx ?=> P[A]): Either[ParseError, A] =
      parse(input.toCharArray, rule)

    def runLex[A](input: String)(rule: LexCtx ?=> A): Either[ParseError, A] =
      val ctx = new LexCtx(input)
      given LexCtx = ctx
      val result = rule
      if !ctx.ok then Left(ParseError(ctx.capturePos(), ctx.failMsg))
      else Right(result)

  // --- Character primitives ---

  "char matches exact character" in {
    L.parseChars("a") { L.char('a') } shouldBe Right('a')
  }

  "char fails on wrong character" in {
    L.parseChars("b") { L.char('a') } shouldBe a[Left[?, ?]]
  }

  "charIn matches any listed character" in {
    L.parseChars("+") { L.charIn("+-*/") } shouldBe Right('+')
    L.parseChars("*") { L.charIn("+-*/") } shouldBe Right('*')
  }

  "charIn fails on unlisted character" in {
    L.parseChars("x") { L.charIn("+-*/") } shouldBe a[Left[?, ?]]
  }

  "charWhere matches by predicate" in {
    L.parseChars("A") { L.charWhere(_.isUpper, "uppercase") } shouldBe Right('A')
  }

  "charWhere fails when predicate is false" in {
    L.parseChars("a") { L.charWhere(_.isUpper, "uppercase") } shouldBe a[Left[?, ?]]
  }

  "str matches exact string" in {
    L.parseChars("hello") { L.str("hello") } shouldBe Right("hello")
  }

  "str fails on partial match" in {
    L.parseChars("help") { L.str("hello") } shouldBe a[Left[?, ?]]
  }

  "str fails on empty input" in {
    L.parseChars("") { L.str("hello") } shouldBe a[Left[?, ?]]
  }

  // --- Identifier ---

  "identifier matches simple word" in {
    L.parseChars("foo") { L.identifier(_.isLetter, _.isLetterOrDigit) } shouldBe Right("foo")
  }

  "identifier matches with digits after start" in {
    L.parseChars("x42") { L.identifier(_.isLetter, _.isLetterOrDigit) } shouldBe Right("x42")
  }

  "identifier fails if start char doesn't match" in {
    L.parseChars("42x") { L.identifier(_.isLetter, _.isLetterOrDigit) } shouldBe a[Left[?, ?]]
  }

  "identifier with underscore" in {
    L.parseChars("_foo_bar") {
      L.identifier(c => c.isLetter || c == '_', c => c.isLetterOrDigit || c == '_')
    } shouldBe Right("_foo_bar")
  }

  // --- Number literals ---

  "digits matches one digit" in {
    L.parseChars("5") { L.digits } shouldBe Right("5")
  }

  "digits matches multiple digits" in {
    L.parseChars("42") { L.digits } shouldBe Right("42")
  }

  "digits fails on non-digit" in {
    L.parseChars("abc") { L.digits } shouldBe a[Left[?, ?]]
  }

  "integerLit matches number" in {
    L.parseChars("123") { L.integerLit } shouldBe Right("123")
  }

  "decimalLit matches integer" in {
    L.parseChars("42") { L.decimalLit } shouldBe Right("42")
  }

  "decimalLit matches decimal" in {
    L.parseChars("3.14") { L.decimalLit } shouldBe Right("3.14")
  }

  "decimalLit fails on lone dot" in {
    L.parseChars(".5") { L.decimalLit } shouldBe a[Left[?, ?]]
  }

  // --- String literals ---

  "stringLit matches simple string" in {
    L.parseChars("'hello'") { L.stringLit('\'', '\\') } shouldBe Right("hello")
  }

  "stringLit matches empty string" in {
    L.parseChars("''") { L.stringLit('\'', '\\') } shouldBe Right("")
  }

  "stringLit handles escape sequences" in {
    L.parseChars("'a\\nb'") { L.stringLit('\'', '\\') } shouldBe Right("a\nb")
  }

  "stringLit handles escaped quote" in {
    L.parseChars("'it\\'s'") { L.stringLit('\'', '\\') } shouldBe Right("it's")
  }

  "stringLit handles tab escape" in {
    L.parseChars("'a\\tb'") { L.stringLit('\'', '\\') } shouldBe Right("a\tb")
  }

  "stringLit handles escaped backslash" in {
    L.parseChars("'a\\\\b'") { L.stringLit('\'', '\\') } shouldBe Right("a\\b")
  }

  "stringLit fails on unclosed string" in {
    L.parseChars("'hello") { L.stringLit('\'', '\\') } shouldBe a[Left[?, ?]]
  }

  "stringLit with double quotes" in {
    L.parseChars("\"hello\"") { L.stringLit('"', '\\') } shouldBe Right("hello")
  }

  // --- Whitespace ---

  "whitespace skips spaces and tabs" in {
    L.runLex("   abc") {
      L.whitespace
      summon[L.LexCtx].capturePos().col
    } shouldBe Right(4)
  }

  "whitespace skips newlines" in {
    L.runLex("  \n  abc") {
      L.whitespace
      val pos = summon[L.LexCtx].capturePos()
      (pos.line, pos.col)
    } shouldBe Right((2, 3))
  }

  "whitespace is ok on empty input" in {
    L.runLex("") {
      L.whitespace
      ()
    } shouldBe Right(())
  }

  // --- Line comments ---

  "lineComment skips to end of line" in {
    L.runLex("// comment\nabc") {
      L.lineComment("//")
      val ctx = summon[L.LexCtx]
      ctx.tokens(ctx.index)
    } shouldBe Right('\n')
  }

  "lineComment is no-op when prefix doesn't match" in {
    L.runLex("abc") {
      L.lineComment("//")
      summon[L.LexCtx].index
    } shouldBe Right(0)
  }

  // --- Block comments ---

  "blockComment skips simple block" in {
    L.runLex("/* comment */abc") {
      L.blockComment("/*", "*/", nested = false)
      summon[L.LexCtx].tokens(summon[L.LexCtx].index)
    } shouldBe Right('a')
  }

  "blockComment handles nested when enabled" in {
    L.runLex("/* outer /* inner */ end */abc") {
      L.blockComment("/*", "*/", nested = true)
      summon[L.LexCtx].tokens(summon[L.LexCtx].index)
    } shouldBe Right('a')
  }

  "blockComment without nesting stops at first close" in {
    L.runLex("/* outer /* inner */ end */abc") {
      L.blockComment("/*", "*/", nested = false)
      summon[L.LexCtx].tokens(summon[L.LexCtx].index)
    } shouldBe Right(' ')
  }

  "blockComment fails on unclosed" in {
    L.runLex("/* never closed") {
      L.blockComment("/*", "*/", nested = false)
    } shouldBe a[Left[?, ?]]
  }

  "blockComment is no-op when open doesn't match" in {
    L.runLex("abc") {
      L.blockComment("/*", "*/", nested = false)
      summon[L.LexCtx].index
    } shouldBe Right(0)
  }

  // --- skipWhitespace ---

  "skipWhitespace with line comments" in {
    L.runLex("  // comment\n  abc") {
      L.skipWhitespace("//")
      val ctx = summon[L.LexCtx]
      (ctx.capturePos().line, ctx.capturePos().col)
    } shouldBe Right((2, 3))
  }

  "skipWhitespace with block and line comments" in {
    L.runLex("/* block */ // line\nabc") {
      L.skipWhitespace("//", "/*", "*/", false)
      val ctx = summon[L.LexCtx]
      (ctx.capturePos().line, ctx.capturePos().col)
    } shouldBe Right((2, 1))
  }

  // --- Position tracking ---

  "capturePos reports correct line and column" in {
    L.runLex("abc") {
      summon[L.LexCtx].capturePos()
    } shouldBe Right(Pos(1, 1, "abc"))
  }

  "capturePos tracks across newlines" in {
    L.runLex("ab\ncd\nef") {
      val ctx = summon[L.LexCtx]
      ctx.advance(); ctx.advance(); ctx.advance() // past "ab\n"
      ctx.advance() // past "c"
      ctx.capturePos()
    } shouldBe Right(Pos(2, 2, "cd"))
  }

  "capturePos lineText is correct" in {
    L.runLex("first line\nsecond line\nthird") {
      val ctx = summon[L.LexCtx]
      var i = 0; while i < 11 do { ctx.advance(); i += 1 } // past "first line\n"
      ctx.capturePos()
    } shouldBe Right(Pos(2, 1, "second line"))
  }

  // --- Tokenize ---

  case class Tok(kind: String, text: String, pos: Pos)

  "tokenize produces token array" in {
    object TL extends Lexers:
      def nextToken(using ctx: LexCtx): P[Tok] =
        whitespace
        if ctx.atEnd then
          ctx.ok = false
          ctx.failMsg = "unexpected end of input"
          fail
        else
          val pos = ctx.capturePos()
          identifier(_.isLetter, _.isLetterOrDigit) ^^ { text => Tok("ident", text, pos) } |
            digits ^^ { text => Tok("int", text, pos) } |
            charIn("+-") ^^ { c => Tok("op", c.toString, pos) }

    val result = TL.tokenize("foo + 42", TL.nextToken)
    result match
      case Right(tokens) =>
        tokens.length shouldBe 3
        tokens(0).asInstanceOf[Tok].kind shouldBe "ident"
        tokens(0).asInstanceOf[Tok].text shouldBe "foo"
        tokens(0).asInstanceOf[Tok].pos.col shouldBe 1
        tokens(1).asInstanceOf[Tok].kind shouldBe "op"
        tokens(1).asInstanceOf[Tok].text shouldBe "+"
        tokens(1).asInstanceOf[Tok].pos.col shouldBe 5
        tokens(2).asInstanceOf[Tok].kind shouldBe "int"
        tokens(2).asInstanceOf[Tok].text shouldBe "42"
        tokens(2).asInstanceOf[Tok].pos.col shouldBe 7
      case Left(err) => fail(s"tokenize failed: $err")
  }

  "tokenize reports error with position" in {
    object TL extends Lexers:
      def nextToken(using ctx: LexCtx): P[Tok] =
        whitespace
        if ctx.atEnd then
          ctx.ok = false
          fail
        else
          val pos = ctx.capturePos()
          identifier(_.isLetter, _.isLetterOrDigit) ^^ { text => Tok("ident", text, pos) }

    val result = TL.tokenize("foo 42", TL.nextToken)
    result shouldBe a[Left[?, ?]]
  }
