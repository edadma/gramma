package io.github.edadma.gramma

case class ~[+A, +B](a: A, b: B):
  override def toString: String = s"($a ~ $b)"
