package io.github.edadma.gramma

abstract class TokenParsers[Token] extends Parsers[Token]:

  def accept(pred: Token => Boolean, msg: String)(using ctx: ParseCtx): P[Token] =
    doAccept(pred, msg)

  // Subclasses must implement this to extract position from their token type
  def tokenPos(token: Token): Pos

  override protected def positionOf(token: Token): Option[Pos] =
    Some(tokenPos(token))
