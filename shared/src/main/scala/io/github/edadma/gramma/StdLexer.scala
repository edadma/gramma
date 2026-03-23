package io.github.edadma.gramma

import scala.collection.mutable

/** Standard lexer with built-in recognition for identifiers, string literals,
  * numeric literals, keywords, and delimiters. Users declare reserved words
  * and delimiters; the lexer handles everything else automatically.
  *
  * Override `customToken` to add recognition for token types not covered
  * by the standard rules (e.g., regex literals, heredocs).
  */
abstract class StdLexer extends Lexers:

  /** Reserved words — identifiers matching these become Keyword tokens. */
  val reserved: mutable.Set[String] = mutable.Set.empty

  /** Delimiter strings — matched longest-first (e.g., "<=" before "<"). */
  val delimiters: mutable.Set[String] = mutable.Set.empty

  // Sorted longest-first at tokenize time for correct greedy matching
  private lazy val sortedDelimiters: Array[String] =
    delimiters.toArray.sortBy(-_.length)

  // First chars of all delimiters for fast dispatch
  private lazy val delimFirstChars: Set[Char] =
    delimiters.map(_.head).toSet

  /** Override to recognize custom tokens. Return `None` to fall through
    * to the standard rules. Called before standard recognition.
    */
  protected def customToken(using ctx: LexCtx): Option[P[StdToken]] = None

  /** Override to customize identifier start character. Default: letter or underscore. */
  protected def isIdentStart(c: Char): Boolean = c.isLetter || c == '_'

  /** Override to customize identifier continuation character. Default: letter, digit, or underscore. */
  protected def isIdentPart(c: Char): Boolean = c.isLetterOrDigit || c == '_'

  /** Override to customize the string quote character. Default: double quote. */
  protected def stringQuote: Char = '"'

  /** Override to customize the string escape character. Default: backslash. */
  protected def stringEscape: Char = '\\'

  /** Override to customize whitespace/comment skipping. Default: whitespace only. */
  protected def skip(using ctx: LexCtx): Unit = whitespace

  // --- Internal optimized tokenization ---

  def nextToken(using ctx: LexCtx): P[StdToken] =
    skip
    nextTokenAfterSkip

  /** Tokenize one token, assuming whitespace was already skipped.
    * Fails if at end of input — callers should check atEnd first.
    */
  private def nextTokenAfterSkip(using ctx: LexCtx): P[StdToken] =
    // Try custom token first
    customToken match
      case Some(tok) => return tok
      case None      =>

    if ctx.atEnd then
      ctx.ok = false
      return fail

    val pos = ctx.capturePos()
    val c = ctx.tokens(ctx.index)

    // Identifier or keyword
    if isIdentStart(c) then
      return lexIdentOrKeyword(pos)

    // String literal
    if c == stringQuote then
      return lexString(pos)

    // Numeric literal
    if c.isDigit then
      return lexNumber(pos)

    // Delimiter (longest match)
    if delimFirstChars.contains(c) then
      val d = matchDelimiter()
      if d != null then
        return succeed(StdToken(StdTokenKind.Delimiter, d, pos))

    // Nothing matched
    if ctx.index >= ctx.failAt then
      ctx.failAt = ctx.index
      ctx.failMsg = s"unexpected character '$c'"
    ctx.ok = false
    fail

  // --- Optimized internal methods (direct loops, no combinator overhead) ---

  private def lexIdentOrKeyword(pos: Pos)(using ctx: LexCtx): P[StdToken] =
    val start = ctx.index
    ctx.advance() // skip first char (already verified isIdentStart)
    while !ctx.atEnd && isIdentPart(ctx.tokens(ctx.index)) do
      ctx.advance()
    val text = ctx.tokenText(start)
    if reserved.contains(text) then
      succeed(StdToken(StdTokenKind.Keyword, text, pos))
    else
      succeed(StdToken(StdTokenKind.Ident, text, pos))

  private def lexString(pos: Pos)(using ctx: LexCtx): P[StdToken] =
    val text = stringLit(stringQuote, stringEscape)
    if !ctx.ok then return fail
    succeed(StdToken(StdTokenKind.StringLit, extract(text), pos))

  private def lexNumber(pos: Pos)(using ctx: LexCtx): P[StdToken] =
    val start = ctx.index
    // Integer part
    while !ctx.atEnd && ctx.tokens(ctx.index).isDigit do
      ctx.advance()
    // Optional decimal part
    if !ctx.atEnd && ctx.tokens(ctx.index) == '.' then
      ctx.advance()
      if ctx.atEnd || !ctx.tokens(ctx.index).isDigit then
        if ctx.index >= ctx.failAt then
          ctx.failAt = ctx.index
          ctx.failMsg = "expected digit after decimal point"
        ctx.ok = false
        return fail
      while !ctx.atEnd && ctx.tokens(ctx.index).isDigit do
        ctx.advance()
    succeed(StdToken(StdTokenKind.NumericLit, ctx.tokenText(start), pos))

  private def matchDelimiter()(using ctx: LexCtx): String | Null =
    val sd = sortedDelimiters
    var i = 0
    while i < sd.length do
      val d = sd(i)
      if matchesDelimAt(d) then
        var j = 0
        while j < d.length do
          ctx.advance()
          j += 1
        return d
      i += 1
    null

  private def matchesDelimAt(d: String)(using ctx: LexCtx): Boolean =
    if ctx.index + d.length > ctx.tokens.length then return false
    var i = 0
    while i < d.length do
      if ctx.tokens(ctx.index + i) != d.charAt(i) then return false
      i += 1
    // For multi-char delimiters starting with identifier chars, ensure
    // we don't match inside an identifier (e.g., "in" inside "int")
    true

  // --- Tokenize ---

  def tokenize(source: String): Either[ParseError, Array[StdToken]] =
    given ctx: LexCtx = new LexCtx(source)
    val buf = scala.collection.mutable.ArrayBuffer[StdToken]()
    skip
    while !ctx.atEnd do
      val result = nextTokenAfterSkip
      if !ctx.ok then
        return Left(ParseError(ctx.capturePos(), ctx.failMsg))
      buf += extract(result)
      skip
    Right(buf.toArray)
