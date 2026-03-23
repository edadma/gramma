package io.github.edadma.gramma

abstract class Parsers[T]:

  class ParseCtx(val tokens: Array[T]):
    var index: Int = 0
    var ok: Boolean = true
    var failAt: Int = 0
    var failMsg: String = ""

    def advance(): Unit =
      index += 1

    def atEnd: Boolean = index >= tokens.length

  // P[A] is opaque — at runtime it's just A, no wrapper
  opaque type P[A] = A

  // Helpers for subclasses to construct P values across the opaque boundary
  protected def succeed[A](a: A): P[A] = a
  protected def fail[A]: P[A] = null.asInstanceOf[P[A]]

  // --- Abstract primitive ---

  def accept(pred: T => Boolean, msg: String)(using ctx: ParseCtx): P[T]

  // Default implementation of accept — subclasses can use this
  protected def doAccept(pred: T => Boolean, msg: String)(using ctx: ParseCtx): P[T] =
    if !ctx.ok then null.asInstanceOf[P[T]]
    else if ctx.atEnd then
      if ctx.index >= ctx.failAt then
        ctx.failAt = ctx.index
        ctx.failMsg = s"expected $msg but got end of input"
      ctx.ok = false
      null.asInstanceOf[P[T]]
    else
      val tok = ctx.tokens(ctx.index)
      if pred(tok) then
        ctx.advance()
        tok
      else
        if ctx.index >= ctx.failAt then
          ctx.failAt = ctx.index
          ctx.failMsg = s"expected $msg"
        ctx.ok = false
        null.asInstanceOf[P[T]]

  // --- Sequencing ---

  extension [A](a: P[A])
    inline def ~[B](inline b: => P[B])(using ctx: ParseCtx): P[A ~ B] =
      if !ctx.ok then null.asInstanceOf[P[A ~ B]]
      else
        val av = a
        if !ctx.ok then null.asInstanceOf[P[A ~ B]]
        else
          val bv = b
          if !ctx.ok then null.asInstanceOf[P[A ~ B]]
          else new ~(av, bv)

    inline def ~>[B](inline b: => P[B])(using ctx: ParseCtx): P[B] =
      if !ctx.ok then null.asInstanceOf[P[B]]
      else
        val _ = a
        if !ctx.ok then null.asInstanceOf[P[B]]
        else b

    inline def <~[B](inline b: => P[B])(using ctx: ParseCtx): P[A] =
      if !ctx.ok then null.asInstanceOf[P[A]]
      else
        val av = a
        if !ctx.ok then null.asInstanceOf[P[A]]
        else
          val _ = b
          if !ctx.ok then null.asInstanceOf[P[A]]
          else av

  // --- Alternation (committed choice) ---

  extension [A](inline a: => P[A])
    inline def |[B >: A](inline b: => P[B])(using ctx: ParseCtx): P[B] =
      val savedIndex = ctx.index
      val result = a
      if ctx.ok then result
      else if ctx.index > savedIndex then result // consumed input — committed, propagate failure
      else
        ctx.ok = true
        b

  // --- Mapping ---

  extension [A](a: P[A])
    inline def ^^[B](inline f: A => B)(using ctx: ParseCtx): P[B] =
      if !ctx.ok then null.asInstanceOf[P[B]]
      else f(a)

  // --- Repetition ---

  inline def rep[A](inline p: => P[A])(using ctx: ParseCtx): P[List[A]] =
    val buf = scala.collection.mutable.ListBuffer[A]()
    var continue = true
    var hardFail = false
    while continue && !hardFail do
      val savedIndex = ctx.index
      val v = p
      if ctx.ok then buf += v
      else if ctx.index > savedIndex then
        hardFail = true
      else
        ctx.ok = true
        continue = false
    if hardFail then null.asInstanceOf[P[List[A]]]
    else buf.toList

  inline def rep1[A](inline p: => P[A])(using ctx: ParseCtx): P[List[A]] =
    val first = p
    if !ctx.ok then null.asInstanceOf[P[List[A]]]
    else
      val rest = rep(p)
      if !ctx.ok then null.asInstanceOf[P[List[A]]]
      else first :: rest

  inline def repsep[A](inline p: => P[A], inline sep: => P[Any])(using ctx: ParseCtx): P[List[A]] =
    val savedIndex = ctx.index
    val first = p
    if !ctx.ok then
      if ctx.index > savedIndex then null.asInstanceOf[P[List[A]]]
      else
        ctx.ok = true
        Nil
    else
      val buf = scala.collection.mutable.ListBuffer[A](first)
      var continue = true
      var hardFail = false
      while continue && !hardFail do
        val sepIndex = ctx.index
        val _ = sep
        if !ctx.ok then
          if ctx.index > sepIndex then hardFail = true
          else
            ctx.ok = true
            continue = false
        else
          val v = p
          if !ctx.ok then hardFail = true
          else buf += v
      if hardFail then null.asInstanceOf[P[List[A]]]
      else buf.toList

  inline def rep1sep[A](inline p: => P[A], inline sep: => P[Any])(using ctx: ParseCtx): P[List[A]] =
    val first = p
    if !ctx.ok then null.asInstanceOf[P[List[A]]]
    else
      val buf = scala.collection.mutable.ListBuffer[A](first)
      var continue = true
      var hardFail = false
      while continue && !hardFail do
        val sepIndex = ctx.index
        val _ = sep
        if !ctx.ok then
          if ctx.index > sepIndex then hardFail = true
          else
            ctx.ok = true
            continue = false
        else
          val v = p
          if !ctx.ok then hardFail = true
          else buf += v
      if hardFail then null.asInstanceOf[P[List[A]]]
      else buf.toList

  // --- Optional ---

  inline def opt[A](inline p: => P[A])(using ctx: ParseCtx): P[Option[A]] =
    val savedIndex = ctx.index
    val v = p
    if ctx.ok then Some(v)
    else if ctx.index > savedIndex then null.asInstanceOf[P[Option[A]]]
    else
      ctx.ok = true
      None

  // --- Lookahead ---

  inline def peek[A](inline p: => P[A])(using ctx: ParseCtx): Boolean =
    val savedIndex = ctx.index
    val savedOk = ctx.ok
    val _ = p
    val matched = ctx.ok
    ctx.index = savedIndex
    ctx.ok = savedOk
    matched

  inline def not[A](inline p: => P[A])(using ctx: ParseCtx): Unit =
    val savedIndex = ctx.index
    val savedOk = ctx.ok
    val _ = p
    val matched = ctx.ok
    ctx.index = savedIndex
    if matched then
      ctx.ok = false
      if ctx.index >= ctx.failAt then
        ctx.failAt = ctx.index
        ctx.failMsg = "unexpected match in negative lookahead"
    else
      ctx.ok = savedOk

  // --- Left-associative expression parsing ---

  inline def leftAssoc[A](inline p: => P[A], inline op: => P[String])(f: (A, String, A) => A)(using ctx: ParseCtx): P[A] =
    val first = p
    if !ctx.ok then null.asInstanceOf[P[A]]
    else
      var result = first
      var continue = true
      var hardFail = false
      while continue && !hardFail do
        val savedIndex = ctx.index
        val o = op
        if !ctx.ok then
          if ctx.index > savedIndex then hardFail = true
          else
            ctx.ok = true
            continue = false
        else
          val right = p
          if !ctx.ok then hardFail = true
          else result = f(result, o, right)
      if hardFail then null.asInstanceOf[P[A]]
      else result

  // --- Positioned ---

  inline def positioned[A <: Positional](inline p: => P[A])(using ctx: ParseCtx): P[A] =
    val startIndex = ctx.index
    val result = p
    if ctx.ok && startIndex < ctx.tokens.length then
      positionOf(ctx.tokens(startIndex)).foreach(result.setPos)
    result

  // Override in TokenParsers to extract Pos from tokens
  protected def positionOf(token: T): Option[Pos] = None

  // --- Entry point ---

  def parse[A](tokens: Array[T], rule: ParseCtx ?=> P[A]): Either[ParseError, A] =
    given ctx: ParseCtx = new ParseCtx(tokens)
    val result = rule
    if !ctx.ok then
      val pos =
        if ctx.failAt < tokens.length then
          positionOf(tokens(ctx.failAt)).getOrElse(Pos(0, 0, ""))
        else if tokens.nonEmpty then
          positionOf(tokens(tokens.length - 1)).getOrElse(Pos(0, 0, ""))
        else
          Pos(0, 0, "")
      Left(ParseError(pos, ctx.failMsg))
    else if !ctx.atEnd then
      val pos =
        if ctx.index < tokens.length then
          positionOf(tokens(ctx.index)).getOrElse(Pos(0, 0, ""))
        else
          Pos(0, 0, "")
      Left(ParseError(pos, s"expected end of input"))
    else
      Right(result)
