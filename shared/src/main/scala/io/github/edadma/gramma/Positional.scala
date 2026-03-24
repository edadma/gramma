package io.github.edadma.gramma

trait Positional:
  var pos: Pos = Pos(0, "")

  def setPos(p: Pos): this.type =
    pos = p
    this
