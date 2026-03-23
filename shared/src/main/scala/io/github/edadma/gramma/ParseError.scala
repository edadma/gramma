package io.github.edadma.gramma

case class ParseError(pos: Pos, msg: String):
  override def toString: String =
    s"${pos.line}:${pos.col}: $msg\n${pos.lineText}\n${" " * (pos.col - 1)}^"
