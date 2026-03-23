package io.github.edadma.gramma

enum StdTokenKind:
  case Ident, StringLit, NumericLit, Keyword, Delimiter
  case Indent, Dedent, Newline

case class StdToken(kind: StdTokenKind, text: String, pos: Pos)
