package io.github.edadma.gramma

abstract class Lexers extends Parsers[Char]:

  class LexCtx(val source: String) extends ParseCtx(source.toCharArray):
    var line: Int = 1
    var col: Int = 1
    private var lineStart: Int = 0
    private var lineEnd: Int = source.indexOf('\n') match
      case -1 => source.length
      case n  => n
    private var lineTextCache: String = null

    override def advance(): Unit =
      if tokens(index) == '\n' then
        line += 1
        col = 1
        lineStart = index + 1
        lineEnd = source.indexOf('\n', lineStart) match
          case -1 => source.length
          case n  => n
        lineTextCache = null
      else
        col += 1
      super.advance()

    def capturePos(): Pos =
      if lineTextCache == null then
        lineTextCache = source.substring(lineStart, lineEnd)
      Pos(line, col, lineTextCache)

    def tokenText(start: Int): String =
      source.substring(start, index)

  // Implement accept for Char tokens
  def accept(pred: Char => Boolean, msg: => String)(using ctx: ParseCtx): P[Char] =
    doAccept(pred, msg)

  // --- Character primitives ---

  def char(c: Char)(using ctx: ParseCtx): P[Char] =
    accept(_ == c, s"'$c'")

  def charIn(chars: String)(using ctx: ParseCtx): P[Char] =
    accept(c => chars.indexOf(c) >= 0, s"one of '$chars'")

  def charWhere(pred: Char => Boolean, msg: String)(using ctx: ParseCtx): P[Char] =
    accept(pred, msg)

  def str(s: String)(using ctx: ParseCtx): P[String] =
    var i = 0
    while i < s.length do
      if ctx.atEnd || ctx.tokens(ctx.index) != s.charAt(i) then
        if ctx.index >= ctx.failAt then
          ctx.failAt = ctx.index
          ctx.failMsg = s"expected \"$s\""
        ctx.ok = false
        return fail
      ctx.advance()
      i += 1
    succeed(s)

  // --- Lexer convenience methods ---

  def identifier(start: Char => Boolean, rest: Char => Boolean)(using ctx: ParseCtx): P[String] =
    val begin = ctx.index
    val first = accept(start, "identifier start")
    if !ctx.ok then return fail
    while !ctx.atEnd && rest(ctx.tokens(ctx.index)) do
      ctx.advance()
    ctx match
      case lctx: LexCtx => succeed(lctx.tokenText(begin))
      case _             => succeed(new String(ctx.tokens.slice(begin, ctx.index).asInstanceOf[Array[Char]]))

  def digits(using ctx: ParseCtx): P[String] =
    val start = ctx.index
    val first = accept(_.isDigit, "digit")
    if !ctx.ok then return fail
    while !ctx.atEnd && ctx.tokens(ctx.index).isDigit do
      ctx.advance()
    ctx match
      case lctx: LexCtx => succeed(lctx.tokenText(start))
      case _             => succeed(new String(ctx.tokens.slice(start, ctx.index).asInstanceOf[Array[Char]]))

  def integerLit(using ctx: ParseCtx): P[String] =
    digits

  def decimalLit(using ctx: ParseCtx): P[String] =
    val start = ctx.index
    val whole = digits
    if !ctx.ok then return fail
    // Check for decimal point
    if !ctx.atEnd && ctx.tokens(ctx.index) == '.' then
      ctx.advance()
      val frac = digits
      if !ctx.ok then return fail
    ctx match
      case lctx: LexCtx => succeed(lctx.tokenText(start))
      case _             => succeed(new String(ctx.tokens.slice(start, ctx.index).asInstanceOf[Array[Char]]))

  def stringLit(quote: Char, escape: Char)(using ctx: ParseCtx): P[String] =
    val sb = new StringBuilder
    val q = char(quote)
    if !ctx.ok then return fail
    while !ctx.atEnd && ctx.tokens(ctx.index) != quote do
      if ctx.tokens(ctx.index) == escape then
        ctx.advance()
        if ctx.atEnd then
          if ctx.index >= ctx.failAt then
            ctx.failAt = ctx.index
            ctx.failMsg = "unexpected end of input in string literal"
          ctx.ok = false
          return fail
        val escaped = ctx.tokens(ctx.index)
        val ch = escaped match
          case 'n'  => '\n'
          case 't'  => '\t'
          case 'r'  => '\r'
          case '\\' => '\\'
          case c if c == quote => quote
          case c if c == escape => escape
          case c    => c
        sb.append(ch)
        ctx.advance()
      else
        sb.append(ctx.tokens(ctx.index))
        ctx.advance()
    // Consume closing quote
    val close = char(quote)
    if !ctx.ok then
      if ctx.index >= ctx.failAt then
        ctx.failAt = ctx.index
        ctx.failMsg = s"unclosed string literal, expected '$quote'"
      return fail
    succeed(sb.toString)

  def whitespace(using ctx: ParseCtx): P[Unit] =
    while !ctx.atEnd && (ctx.tokens(ctx.index) == ' ' || ctx.tokens(ctx.index) == '\t' ||
      ctx.tokens(ctx.index) == '\n' || ctx.tokens(ctx.index) == '\r') do
      ctx.advance()
    succeed(())

  def lineComment(start: String)(using ctx: ParseCtx): P[Unit] =
    val savedIndex = ctx.index
    val s = str(start)
    if !ctx.ok then
      ctx.index = savedIndex
      ctx.ok = true
      return succeed(())
    while !ctx.atEnd && ctx.tokens(ctx.index) != '\n' do
      ctx.advance()
    succeed(())

  def blockComment(open: String, close: String, nested: Boolean)(using ctx: ParseCtx): P[Unit] =
    val savedIndex = ctx.index
    val o = str(open)
    if !ctx.ok then
      ctx.index = savedIndex
      ctx.ok = true
      return succeed(())
    var depth = 1
    while !ctx.atEnd && depth > 0 do
      if nested && matchesAt(open) then
        var i = 0
        while i < open.length do
          ctx.advance()
          i += 1
        depth += 1
      else if matchesAt(close) then
        var i = 0
        while i < close.length do
          ctx.advance()
          i += 1
        depth -= 1
      else
        ctx.advance()
    if depth > 0 then
      if ctx.index >= ctx.failAt then
        ctx.failAt = ctx.index
        ctx.failMsg = s"unclosed block comment, expected '$close'"
      ctx.ok = false
      fail
    else
      succeed(())

  private def matchesAt(s: String)(using ctx: ParseCtx): Boolean =
    var i = 0
    while i < s.length do
      if ctx.index + i >= ctx.tokens.length || ctx.tokens(ctx.index + i) != s.charAt(i) then
        return false
      i += 1
    true

  def skipWhitespace(using ctx: ParseCtx): P[Unit] =
    whitespace

  def skipWhitespace(lineCommentStart: String)(using ctx: ParseCtx): P[Unit] =
    var continue = true
    while continue do
      val saved = ctx.index
      whitespace
      lineComment(lineCommentStart)
      if ctx.index == saved then continue = false
    succeed(())

  def skipWhitespace(lineCommentStart: String, blockOpen: String, blockClose: String, nestedComments: Boolean)(using ctx: ParseCtx): P[Unit] =
    var continue = true
    while continue do
      val saved = ctx.index
      whitespace
      lineComment(lineCommentStart)
      blockComment(blockOpen, blockClose, nestedComments)
      if ctx.index == saved then continue = false
    succeed(())

  // --- Tokenize entry point ---

  def tokenize[Tok: scala.reflect.ClassTag](source: String, rule: LexCtx ?=> P[Tok]): Either[ParseError, Array[Tok]] =
    given ctx: LexCtx = new LexCtx(source)
    val buf = scala.collection.mutable.ArrayBuffer[Tok]()
    while !ctx.atEnd do
      val result = rule
      if !ctx.ok then
        return Left(ParseError(ctx.capturePos(), ctx.failMsg))
      buf += extract(result)
    Right(buf.toArray)
