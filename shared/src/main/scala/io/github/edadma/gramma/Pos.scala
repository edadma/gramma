package io.github.edadma.gramma

/** Source position stored as a raw character offset into the source string.
  * Line number, column, and line text are computed lazily — only when
  * an error message is actually formatted — so lexing never allocates
  * substrings or tracks line/col incrementally.
  */
case class Pos(offset: Int, source: String):
  private lazy val computed: (Int, Int, String) =
    var l         = 1
    var lineStart = 0
    var i         = 0
    val limit     = offset.min(source.length)
    while i < limit do
      if source(i) == '\n' then
        l += 1
        lineStart = i + 1
      i += 1
    val lineEnd = source.indexOf('\n', lineStart) match
      case -1 => source.length
      case n  => n
    (l, offset - lineStart + 1, source.substring(lineStart, lineEnd))

  def line: Int        = computed._1
  def col: Int         = computed._2
  def lineText: String = computed._3
