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
    val ctx = new ParseCtx()
    val lpc = lexCtx.asInstanceOf[lexer.ParseCtx]

    ctx.initLazy { () =>
      if lpc.atEnd then false
      else
        given lc: lexer.LexCtx = lexCtx.asInstanceOf[lexer.LexCtx]
        val result = lexRule
        if !lpc.ok then false
        else
          ctx.appendToken(lexer.extractValue(result))
          true
    }

    runParse(ctx, rule)
