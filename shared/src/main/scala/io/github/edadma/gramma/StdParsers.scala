package io.github.edadma.gramma

/** Standard token parsers with built-in matchers for identifiers, string
  * literals, numeric literals, keywords, and delimiters. Pairs with a
  * StdLexer for automatic tokenization.
  *
  * Usage:
  * {{{
  * object MyParser extends StdParsers(MyLexer):
  *   def expr(using ctx: ParseCtx): P[Expr] =
  *     ident ^^ { name => VarExpr(name) } |
  *       numericLit ^^ { n => NumExpr(n.toInt) } |
  *       keyword("if") ~> expr ~ (keyword("then") ~> expr)
  * }}}
  */
abstract class StdParsers(val lexer: StdLexer) extends TokenParsers[StdToken]:

  def tokenPos(token: StdToken): Pos = token.pos

  // --- Specialized matchers: direct field checks, no closures, no accept overhead ---

  private def matchToken(kind: StdTokenKind, msg: => String)(using ctx: ParseCtx): StdToken | Null =
    if !ctx.ok then null
    else if ctx.atEnd then
      if ctx.index >= ctx.failAt then
        ctx.failAt = ctx.index
        ctx.failMsg = s"expected $msg but got end of input"
      ctx.ok = false
      null
    else
      val tok = ctx.tokenAt(ctx.index)
      if tok.kind == kind then
        ctx.advance()
        tok
      else
        if ctx.index >= ctx.failAt then
          ctx.failAt = ctx.index
          ctx.failMsg = s"expected $msg"
        ctx.ok = false
        null

  private def matchTokenText(kind: StdTokenKind, text: String, msg: => String)(using ctx: ParseCtx): StdToken | Null =
    if !ctx.ok then null
    else if ctx.atEnd then
      if ctx.index >= ctx.failAt then
        ctx.failAt = ctx.index
        ctx.failMsg = s"expected $msg but got end of input"
      ctx.ok = false
      null
    else
      val tok = ctx.tokenAt(ctx.index)
      if tok.kind == kind && tok.text == text then
        ctx.advance()
        tok
      else
        if ctx.index >= ctx.failAt then
          ctx.failAt = ctx.index
          ctx.failMsg = s"expected $msg"
        ctx.ok = false
        null

  /** Match an identifier token, returning its text. */
  def ident(using ctx: ParseCtx): P[String] =
    val tok = matchToken(StdTokenKind.Ident, "identifier")
    if tok != null then succeed(tok.text) else fail

  /** Match a string literal token, returning the string content. */
  def stringLit(using ctx: ParseCtx): P[String] =
    val tok = matchToken(StdTokenKind.StringLit, "string literal")
    if tok != null then succeed(tok.text) else fail

  /** Match a numeric literal token, returning the text. */
  def numericLit(using ctx: ParseCtx): P[String] =
    val tok = matchToken(StdTokenKind.NumericLit, "numeric literal")
    if tok != null then succeed(tok.text) else fail

  /** Match a specific keyword. */
  def keyword(word: String)(using ctx: ParseCtx): P[String] =
    val tok = matchTokenText(StdTokenKind.Keyword, word, s"'$word'")
    if tok != null then succeed(tok.text) else fail

  /** Match a specific delimiter. */
  def delimiter(d: String)(using ctx: ParseCtx): P[String] =
    val tok = matchTokenText(StdTokenKind.Delimiter, d, s"'$d'")
    if tok != null then succeed(tok.text) else fail

  /** Parse source string — tokenizes with the paired lexer then parses. */
  def parseSource[A](source: String)(rule: ParseCtx ?=> P[A]): Either[ParseError, A] =
    for
      tokens <- lexer.tokenize(source)
      ast    <- parse(tokens, rule)
    yield ast

  /** Parse pre-tokenized input. */
  def parseTokens[A](tokens: Array[StdToken])(rule: ParseCtx ?=> P[A]): Either[ParseError, A] =
    parse(tokens, rule)
