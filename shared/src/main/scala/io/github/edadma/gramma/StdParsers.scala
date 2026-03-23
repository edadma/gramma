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

  /** Match an identifier token, returning its text. */
  def ident(using ctx: ParseCtx): P[String] =
    accept(_.kind == StdTokenKind.Ident, "identifier") ^^ { _.text }

  /** Match a string literal token, returning the string content. */
  def stringLit(using ctx: ParseCtx): P[String] =
    accept(_.kind == StdTokenKind.StringLit, "string literal") ^^ { _.text }

  /** Match a numeric literal token, returning the text. */
  def numericLit(using ctx: ParseCtx): P[String] =
    accept(_.kind == StdTokenKind.NumericLit, "numeric literal") ^^ { _.text }

  /** Match a specific keyword. */
  def keyword(word: String)(using ctx: ParseCtx): P[String] =
    accept(t => t.kind == StdTokenKind.Keyword && t.text == word, s"'$word'") ^^ { _.text }

  /** Match a specific delimiter. */
  def delimiter(d: String)(using ctx: ParseCtx): P[String] =
    accept(t => t.kind == StdTokenKind.Delimiter && t.text == d, s"'$d'") ^^ { _.text }

  /** Parse source string — tokenizes with the paired lexer then parses. */
  def parseSource[A](source: String)(rule: ParseCtx ?=> P[A]): Either[ParseError, A] =
    for
      tokens <- lexer.tokenize(source)
      ast    <- parse(tokens, rule)
    yield ast

  /** Parse pre-tokenized input. */
  def parseTokens[A](tokens: Array[StdToken])(rule: ParseCtx ?=> P[A]): Either[ParseError, A] =
    parse(tokens, rule)
