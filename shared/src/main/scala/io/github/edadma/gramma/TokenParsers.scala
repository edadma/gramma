package io.github.edadma.gramma

abstract class TokenParsers[Token] extends Parsers[Token]:

  def accept(pred: Token => Boolean, msg: => String)(using ctx: ParseCtx): P[Token] =
    doAccept(pred, msg)

  // Subclasses must implement this to extract position from their token type
  def tokenPos(token: Token): Pos

  override protected def positionOf(token: Token): Option[Pos] =
    Some(tokenPos(token))

  /** Parse source string by driving the lexer on demand — no intermediate token array. */
  def parseSource[A](source: String, lexer: Lexers, lexRule: lexer.LexCtx ?=> lexer.P[Token])(
      rule: ParseCtx ?=> P[A],
  ): Either[ParseError, A] =
    val lexCtx = new lexer.LexCtx(source)
    val ctx = new LazyParseCtx(lexCtx, lexer, lexRule)
    runParse(ctx, rule)

  private class LazyParseCtx(
      lexCtx: Lexers#LexCtx,
      lexer: Lexers,
      lexRule: lexer.LexCtx ?=> lexer.P[Token],
  ) extends ParseCtx():
    private val buf = scala.collection.mutable.ArrayBuffer[Token]()
    private var exhausted = false

    private def produce(): Boolean =
      if exhausted then return false
      val lpc = lexCtx.asInstanceOf[lexer.ParseCtx]
      if lpc.atEnd then
        exhausted = true
        false
      else
        given lc: lexer.LexCtx = lexCtx.asInstanceOf[lexer.LexCtx]
        val result = lexRule
        if !lpc.ok then
          exhausted = true
          false
        else
          buf += lexer.extractValue(result).asInstanceOf[Token]
          true

    private def ensure(i: Int): Unit =
      while buf.length <= i && produce() do ()

    override def atEnd: Boolean =
      ensure(index)
      index >= buf.length

    override def tokenAt(i: Int): Token =
      ensure(i)
      buf(i)

    override def size: Int =
      buf.length
