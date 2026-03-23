package io.github.edadma.gramma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CombinatorTests extends AnyFreeSpec with Matchers:

  // Minimal lexer harness — just char-level parsing to exercise combinators
  object L extends Lexers:
    def ch(c: Char)(using ctx: ParseCtx): P[Char] = char(c)
    def letter(using ctx: ParseCtx): P[Char] = charWhere(_.isLetter, "letter")
    def digit(using ctx: ParseCtx): P[Char] = charWhere(_.isDigit, "digit")

    def parseChars[A](input: String)(rule: ParseCtx ?=> P[A]): Either[ParseError, A] =
      parse(input.toCharArray, rule)

  // --- Sequencing ---

  "~ sequences two parsers" in {
    L.parseChars("ab") { L.ch('a') ~ L.ch('b') } shouldBe Right(new ~('a', 'b'))
  }

  "~ fails if left fails" in {
    L.parseChars("xb") { L.ch('a') ~ L.ch('b') } shouldBe a[Left[?, ?]]
  }

  "~ fails if right fails" in {
    L.parseChars("ax") { L.ch('a') ~ L.ch('b') } shouldBe a[Left[?, ?]]
  }

  "~> discards left" in {
    L.parseChars("ab") { L.ch('a') ~> L.ch('b') } shouldBe Right('b')
  }

  "<~ discards right" in {
    L.parseChars("ab") { L.ch('a') <~ L.ch('b') } shouldBe Right('a')
  }

  // --- Alternation ---

  "| tries right on left failure" in {
    L.parseChars("b") { L.ch('a') | L.ch('b') } shouldBe Right('b')
  }

  "| returns left on left success" in {
    L.parseChars("a") { L.ch('a') | L.ch('b') } shouldBe Right('a')
  }

  "| fails if both fail" in {
    L.parseChars("x") { L.ch('a') | L.ch('b') } shouldBe a[Left[?, ?]]
  }

  // --- Committed choice ---

  "| does not try right after left consumes input (committed)" in {
    L.parseChars("ac") { (L.ch('a') ~ L.ch('b')) | (L.ch('a') ~ L.ch('c')) } shouldBe a[Left[?, ?]]
  }

  "| tries right when left consumes nothing" in {
    L.parseChars("b") { L.ch('a') | L.ch('b') } shouldBe Right('b')
  }

  // --- Mapping ---

  "^^ maps successful result" in {
    L.parseChars("a") { L.letter ^^ (_.toUpper) } shouldBe Right('A')
  }

  "^^ propagates failure" in {
    L.parseChars("1") { L.letter ^^ (_.toUpper) } shouldBe a[Left[?, ?]]
  }

  // --- Repetition ---

  "rep matches zero occurrences" in {
    L.parseChars("") { L.rep(L.letter) } shouldBe Right(Nil)
  }

  "rep matches multiple occurrences" in {
    L.parseChars("abc") { L.rep(L.letter) } shouldBe Right(List('a', 'b', 'c'))
  }

  "rep stops at non-matching input" in {
    // rep only consumes letters, leaves '1' — but parse checks all input consumed
    // So wrap: rep(letter) <~ digit to consume everything
    L.parseChars("ab1") { L.rep(L.letter) <~ L.digit } shouldBe Right(List('a', 'b'))
  }

  "rep1 fails on zero occurrences" in {
    L.parseChars("1") { L.rep1(L.letter) } shouldBe a[Left[?, ?]]
  }

  "rep1 matches one occurrence" in {
    L.parseChars("a") { L.rep1(L.letter) } shouldBe Right(List('a'))
  }

  "rep1 matches multiple occurrences" in {
    L.parseChars("abc") { L.rep1(L.letter) } shouldBe Right(List('a', 'b', 'c'))
  }

  // --- Repetition with separator ---

  "repsep matches zero occurrences" in {
    L.parseChars("") { L.repsep(L.letter, L.ch(',')) } shouldBe Right(Nil)
  }

  "repsep matches one occurrence" in {
    L.parseChars("a") { L.repsep(L.letter, L.ch(',')) } shouldBe Right(List('a'))
  }

  "repsep matches multiple with separator" in {
    L.parseChars("a,b,c") { L.repsep(L.letter, L.ch(',')) } shouldBe Right(List('a', 'b', 'c'))
  }

  "repsep commits after separator — fails if element missing after sep" in {
    L.parseChars("a,") { L.repsep(L.letter, L.ch(',')) } shouldBe a[Left[?, ?]]
  }

  "rep1sep fails on zero occurrences" in {
    L.parseChars("") { L.rep1sep(L.letter, L.ch(',')) } shouldBe a[Left[?, ?]]
  }

  "rep1sep matches one occurrence" in {
    L.parseChars("a") { L.rep1sep(L.letter, L.ch(',')) } shouldBe Right(List('a'))
  }

  "rep1sep matches multiple with separator" in {
    L.parseChars("a,b,c") { L.rep1sep(L.letter, L.ch(',')) } shouldBe Right(List('a', 'b', 'c'))
  }

  "rep1sep commits after separator" in {
    L.parseChars("a,") { L.rep1sep(L.letter, L.ch(',')) } shouldBe a[Left[?, ?]]
  }

  // --- Optional ---

  "opt returns Some on match" in {
    L.parseChars("a") { L.opt(L.ch('a')) } shouldBe Right(Some('a'))
  }

  "opt returns None on no match" in {
    // opt doesn't consume, so 'b' remains — need to consume it
    L.parseChars("b") { L.opt(L.ch('a')) <~ L.ch('b') } shouldBe Right(None)
  }

  "opt returns None on empty input" in {
    L.parseChars("") { L.opt(L.ch('a')) } shouldBe Right(None)
  }

  // --- Lookahead ---

  "peek returns true without consuming" in {
    L.parseChars("a") {
      val matched = L.peek(L.ch('a'))
      matched shouldBe true
      L.ch('a')
    } shouldBe Right('a')
  }

  "peek returns false without consuming" in {
    L.parseChars("b") {
      val matched = L.peek(L.ch('a'))
      matched shouldBe false
      L.ch('b')
    } shouldBe Right('b')
  }

  "not succeeds when parser fails" in {
    L.parseChars("ba") {
      L.not(L.ch('a'))
      L.ch('b') ~ L.ch('a')
    } shouldBe Right(new ~('b', 'a'))
  }

  "not fails when parser succeeds" in {
    L.parseChars("ab") {
      L.not(L.ch('a'))
      L.ch('a') ~ L.ch('b')
    } shouldBe a[Left[?, ?]]
  }

  // --- leftAssoc ---

  "leftAssoc parses single operand" in {
    L.parseChars("1") {
      L.leftAssoc(L.digit ^^ (_.toString), L.ch('+') ^^ (_.toString)) { (l, op, r) => s"($l$op$r)" }
    } shouldBe Right("1")
  }

  "leftAssoc parses left-associative chain" in {
    L.parseChars("1+2+3") {
      L.leftAssoc(L.digit ^^ (_.toString), L.ch('+') ^^ (_.toString)) { (l, op, r) => s"($l$op$r)" }
    } shouldBe Right("((1+2)+3)")
  }

  "leftAssoc with multiple operators" in {
    L.parseChars("1+2-3") {
      L.leftAssoc(L.digit ^^ (_.toString), (L.ch('+') | L.ch('-')) ^^ (_.toString)) { (l, op, r) => s"($l$op$r)" }
    } shouldBe Right("((1+2)-3)")
  }

  // --- Furthest failure ---

  "error reports furthest failure position" in {
    val result = L.parseChars("ab") { L.ch('a') ~ L.ch('b') ~ L.ch('c') }
    result match
      case Left(err) => err.msg should include("'c'")
      case Right(_)  => fail("expected parse failure")
  }

  // --- End of input ---

  "parse fails if input not fully consumed" in {
    val result = L.parseChars("ab") { L.ch('a') }
    result match
      case Left(err) => err.msg should include("end of input")
      case Right(_)  => fail("expected unconsumed input error")
  }

  // --- Complex combinations ---

  "sequence + alternation + repetition" in {
    L.parseChars("a,b,a") {
      L.letter ~ L.rep(L.ch(',') ~> (L.ch('a') | L.ch('b')))
    } shouldBe Right(new ~('a', List('b', 'a')))
  }

  "nested opt inside rep" in {
    L.parseChars("a1b2c") {
      L.rep(L.letter ~ L.opt(L.digit))
    } shouldBe Right(List(
      new ~('a', Some('1')),
      new ~('b', Some('2')),
      new ~('c', None),
    ))
  }
