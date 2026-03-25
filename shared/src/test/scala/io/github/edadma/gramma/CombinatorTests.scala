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

  // --- repN ---

  "repN matches exactly N" in {
    L.parseChars("abc") { L.repN(3, L.letter) } shouldBe Right(List('a', 'b', 'c'))
  }

  "repN(0) matches empty" in {
    L.parseChars("") { L.repN(0, L.letter) } shouldBe Right(Nil)
  }

  "repN(1) matches one" in {
    L.parseChars("a") { L.repN(1, L.letter) } shouldBe Right(List('a'))
  }

  "repN fails if fewer than N" in {
    L.parseChars("ab") { L.repN(3, L.letter) } shouldBe a[Left[?, ?]]
  }

  "repN does not consume extra" in {
    L.parseChars("abcd") { L.repN(3, L.letter) <~ L.ch('d') } shouldBe Right(List('a', 'b', 'c'))
  }

  "repN fails on empty input when N > 0" in {
    L.parseChars("") { L.repN(1, L.letter) } shouldBe a[Left[?, ?]]
  }

  "repN propagates committed failure" in {
    // each element is two chars; fails mid-element on second pair — '1' is not a letter
    L.parseChars("aba1") { L.repN(2, L.letter ~ L.letter) } shouldBe a[Left[?, ?]]
  }

  // --- log ---

  "log succeeds transparently" in {
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      L.parseChars("a") { L.log(L.ch('a'), "test-a") }
    } shouldBe Right('a')
    val logged = output.toString
    logged should include("trying test-a")
    logged should include("test-a succeeded")
  }

  "log reports failure transparently" in {
    val output = new java.io.ByteArrayOutputStream()
    Console.withOut(output) {
      L.parseChars("b") { L.log(L.ch('a'), "test-a") }
    } shouldBe a[Left[?, ?]]
    output.toString should include("test-a failed")
  }

  "log does not affect parse result" in {
    val output = new java.io.ByteArrayOutputStream()
    val result = Console.withOut(output) {
      L.parseChars("ab") { L.log(L.ch('a'), "first") ~ L.log(L.ch('b'), "second") }
    }
    result shouldBe Right(new ~('a', 'b'))
  }

  // --- Edge cases: empty input ---

  "~ on empty input fails" in {
    L.parseChars("") { L.ch('a') ~ L.ch('b') } shouldBe a[Left[?, ?]]
  }

  "~> on empty input fails" in {
    L.parseChars("") { L.ch('a') ~> L.ch('b') } shouldBe a[Left[?, ?]]
  }

  "<~ on empty input fails" in {
    L.parseChars("") { L.ch('a') <~ L.ch('b') } shouldBe a[Left[?, ?]]
  }

  "| on empty input fails if both branches fail" in {
    L.parseChars("") { L.ch('a') | L.ch('b') } shouldBe a[Left[?, ?]]
  }

  "^^ on empty input fails" in {
    L.parseChars("") { L.letter ^^ (_.toUpper) } shouldBe a[Left[?, ?]]
  }

  "rep1 on empty input fails" in {
    L.parseChars("") { L.rep1(L.letter) } shouldBe a[Left[?, ?]]
  }

  "peek on empty input returns false" in {
    L.parseChars("") {
      val matched = L.peek(L.ch('a'))
      matched shouldBe false
      L.rep(L.letter) // consume nothing, return Nil
    } shouldBe Right(Nil)
  }

  "not on empty input succeeds (nothing to match)" in {
    L.parseChars("") {
      L.not(L.ch('a'))
      L.rep(L.letter) // consume nothing, return Nil
    } shouldBe Right(Nil)
  }

  // --- Edge cases: chained alternation ---

  "three-way alternation" in {
    L.parseChars("c") { L.ch('a') | L.ch('b') | L.ch('c') } shouldBe Right('c')
  }

  "alternation picks first match" in {
    L.parseChars("a") { L.ch('a') | L.ch('a') } shouldBe Right('a')
  }

  // --- Edge cases: nested repetition ---

  "rep of rep" in {
    // Inner rep consumes all letters, outer rep sees no more letters, stops
    L.parseChars("abc") { L.rep(L.rep1(L.letter)) } shouldBe Right(List(List('a', 'b', 'c')))
  }

  // --- Edge cases: leftAssoc ---

  "leftAssoc with no operators" in {
    L.parseChars("a") {
      L.leftAssoc(L.letter ^^ (_.toString), L.ch('+') ^^ (_.toString)) { (l, op, r) => s"($l$op$r)" }
    } shouldBe Right("a")
  }

  // --- FlatMap (>>) ---

  ">> uses result to select next parser" in {
    // Parse a digit, then that many letters
    L.parseChars("3abc") {
      L.digit >> { d =>
        L.repN(d.asDigit, L.letter)
      }
    } shouldBe Right(List('a', 'b', 'c'))
  }

  ">> propagates failure from first parser" in {
    L.parseChars("xabc") {
      L.digit >> { d =>
        L.repN(d.asDigit, L.letter)
      }
    } shouldBe a[Left[?, ?]]
  }

  ">> propagates failure from selected parser" in {
    L.parseChars("3ab1") {
      L.digit >> { d =>
        L.repN(d.asDigit, L.letter)
      }
    } shouldBe a[Left[?, ?]]
  }

  ">> enables common-prefix factoring" in {
    // Parse 'a' then decide based on what follows: 'b' → "ab", 'c' → "ac"
    L.parseChars("ac") {
      L.ch('a') >> { a =>
        L.ch('b') ^^ (b => s"$a$b") | L.ch('c') ^^ (c => s"$a$c")
      }
    } shouldBe Right("ac")
  }

  ">> common-prefix also works for first alternative" in {
    L.parseChars("ab") {
      L.ch('a') >> { a =>
        L.ch('b') ^^ (b => s"$a$b") | L.ch('c') ^^ (c => s"$a$c")
      }
    } shouldBe Right("ab")
  }

  ">> chains multiple times" in {
    L.parseChars("abc") {
      L.ch('a') >> { a =>
        L.ch('b') >> { b =>
          L.ch('c') ^^ (c => s"$a$b$c")
        }
      }
    } shouldBe Right("abc")
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
