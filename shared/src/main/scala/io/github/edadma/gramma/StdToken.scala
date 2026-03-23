package io.github.edadma.gramma

enum StdTokenKind:
  case Ident, StringLit, NumericLit, Keyword, Delimiter

case class StdToken(kind: StdTokenKind, text: String, pos: Pos)
