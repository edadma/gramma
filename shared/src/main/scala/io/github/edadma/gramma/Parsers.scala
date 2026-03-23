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

  // P[A] is opaque — at runtime it's just A, no wrapper.
  // Combinators are regular defs (not inline) so the opaque type is
  // transparent in their bodies. The real performance wins come from
  // mutable context and zero allocation, not from inlining.
  opaque type P[A] = A

  // Helpers for subclasses to work across the opaque boundary
  protected def succeed[A](a: A): P[A] = a
  protected def fail[A]: P[A] = null.asInstanceOf[P[A]]
  protected def extract[A](p: P[A]): A = p

  // --- Abstract primitive ---

  def accept(pred: T => Boolean, msg: => String)(using ctx: ParseCtx): P[T]

  // Default implementation of accept — subclasses can use this
  protected def doAccept(pred: T => Boolean, msg: => String)(using ctx: ParseCtx): P[T] =
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
    def ~[B](b: => P[B])(using ctx: ParseCtx): P[A ~ B] =
      if !ctx.ok then null.asInstanceOf[P[A ~ B]]
      else
        val av: A = a
        if !ctx.ok then null.asInstanceOf[P[A ~ B]]
        else
          val bv: B = b
          if !ctx.ok then null.asInstanceOf[P[A ~ B]]
          else new ~(av, bv)

    def ~>[B](b: => P[B])(using ctx: ParseCtx): P[B] =
      if !ctx.ok then null.asInstanceOf[P[B]]
      else
        val _: A = a
        if !ctx.ok then null.asInstanceOf[P[B]]
        else b

    def <~[B](b: => P[B])(using ctx: ParseCtx): P[A] =
      if !ctx.ok then null.asInstanceOf[P[A]]
      else
        val av: A = a
        if !ctx.ok then null.asInstanceOf[P[A]]
        else
          val _: B = b
          if !ctx.ok then null.asInstanceOf[P[A]]
          else av

  // --- Alternation (committed choice) ---

  extension [A](a: => P[A])
    def |[B >: A](b: => P[B])(using ctx: ParseCtx): P[B] =
      val savedIndex = ctx.index
      val result: A = a
      if ctx.ok then result
      else if ctx.index > savedIndex then result // consumed input — committed, propagate failure
      else
        ctx.ok = true
        b

  // --- Mapping ---

  extension [A](a: P[A])
    def ^^[B](f: A => B)(using ctx: ParseCtx): P[B] =
      if !ctx.ok then null.asInstanceOf[P[B]]
      else f(a)

  // --- Repetition ---

  def rep[A](p: => P[A])(using ctx: ParseCtx): P[List[A]] =
    val buf = scala.collection.mutable.ListBuffer[A]()
    var continue = true
    var hardFail = false
    while continue && !hardFail do
      val savedIndex = ctx.index
      val v: A = p
      if ctx.ok then buf += v
      else if ctx.index > savedIndex then
        hardFail = true
      else
        ctx.ok = true
        continue = false
    if hardFail then null.asInstanceOf[P[List[A]]]
    else buf.toList

  def rep1[A](p: => P[A])(using ctx: ParseCtx): P[List[A]] =
    val first: A = p
    if !ctx.ok then null.asInstanceOf[P[List[A]]]
    else
      val rest: List[A] = rep(p)
      if !ctx.ok then null.asInstanceOf[P[List[A]]]
      else first :: rest

  def repsep[A, S](p: => P[A], sep: => P[S])(using ctx: ParseCtx): P[List[A]] =
    val savedIndex = ctx.index
    val first: A = p
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
        val _: S = sep
        if !ctx.ok then
          if ctx.index > sepIndex then hardFail = true
          else
            ctx.ok = true
            continue = false
        else
          val v: A = p
          if !ctx.ok then hardFail = true
          else buf += v
      if hardFail then null.asInstanceOf[P[List[A]]]
      else buf.toList

  def rep1sep[A, S](p: => P[A], sep: => P[S])(using ctx: ParseCtx): P[List[A]] =
    val first: A = p
    if !ctx.ok then null.asInstanceOf[P[List[A]]]
    else
      val buf = scala.collection.mutable.ListBuffer[A](first)
      var continue = true
      var hardFail = false
      while continue && !hardFail do
        val sepIndex = ctx.index
        val _: S = sep
        if !ctx.ok then
          if ctx.index > sepIndex then hardFail = true
          else
            ctx.ok = true
            continue = false
        else
          val v: A = p
          if !ctx.ok then hardFail = true
          else buf += v
      if hardFail then null.asInstanceOf[P[List[A]]]
      else buf.toList

  // --- Optional ---

  def opt[A](p: => P[A])(using ctx: ParseCtx): P[Option[A]] =
    val savedIndex = ctx.index
    val v: A = p
    if ctx.ok then Some(v)
    else if ctx.index > savedIndex then null.asInstanceOf[P[Option[A]]]
    else
      ctx.ok = true
      None

  // --- Lookahead ---

  def peek[A](p: => P[A])(using ctx: ParseCtx): Boolean =
    val savedIndex = ctx.index
    val savedOk = ctx.ok
    val _: A = p
    val matched = ctx.ok
    ctx.index = savedIndex
    ctx.ok = savedOk
    matched

  def not[A](p: => P[A])(using ctx: ParseCtx): Unit =
    val savedIndex = ctx.index
    val savedOk = ctx.ok
    val _: A = p
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

  def leftAssoc[A](p: => P[A], op: => P[String])(f: (A, String, A) => A)(using ctx: ParseCtx): P[A] =
    val first: A = p
    if !ctx.ok then null.asInstanceOf[P[A]]
    else
      var result: A = first
      var continue = true
      var hardFail = false
      while continue && !hardFail do
        val savedIndex = ctx.index
        val o: String = op
        if !ctx.ok then
          if ctx.index > savedIndex then hardFail = true
          else
            ctx.ok = true
            continue = false
        else
          val right: A = p
          if !ctx.ok then hardFail = true
          else result = f(result, o, right)
      if hardFail then null.asInstanceOf[P[A]]
      else result

  // --- Positioned ---

  def positioned[A <: Positional](p: => P[A])(using ctx: ParseCtx): P[A] =
    val startIndex = ctx.index
    val result: A = p
    if ctx.ok && startIndex < ctx.tokens.length then
      positionOf(ctx.tokens(startIndex)).foreach(result.setPos)
    result

  // Override in TokenParsers to extract Pos from tokens
  protected def positionOf(token: T): Option[Pos] = None

  // --- Entry point ---

  def parse[A](tokens: Array[T], rule: ParseCtx ?=> P[A]): Either[ParseError, A] =
    given ctx: ParseCtx = new ParseCtx(tokens)
    val result: A = rule
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
