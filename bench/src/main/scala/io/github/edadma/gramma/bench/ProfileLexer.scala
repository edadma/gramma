package io.github.edadma.gramma.bench

import io.github.edadma.gramma.*

// Optimized lexer: dispatch on first character instead of alternation chain
object OptimizedJSONLexer extends Lexers:
  def nextToken(using ctx: LexCtx): P[GToken] =
    whitespace
    if ctx.atEnd then
      ctx.ok = false
      fail
    else
      val pos = ctx.capturePos()
      val c = ctx.tokens(ctx.index)
      c match
        case '"' =>
          stringLit('"', '\\') ^^ { text => GToken(GTokenKind.StringLit, text, pos) }
        case c if c.isDigit =>
          decimalLit ^^ { text => GToken(GTokenKind.NumberLit, text, pos) }
        case 't' =>
          str("true") ^^ { _ => GToken(GTokenKind.True, "true", pos) }
        case 'f' =>
          str("false") ^^ { _ => GToken(GTokenKind.False, "false", pos) }
        case 'n' =>
          str("null") ^^ { _ => GToken(GTokenKind.Null, "null", pos) }
        case '{' => ctx.advance(); succeed(GToken(GTokenKind.LBrace, "{", pos))
        case '}' => ctx.advance(); succeed(GToken(GTokenKind.RBrace, "}", pos))
        case '[' => ctx.advance(); succeed(GToken(GTokenKind.LBracket, "[", pos))
        case ']' => ctx.advance(); succeed(GToken(GTokenKind.RBracket, "]", pos))
        case ':' => ctx.advance(); succeed(GToken(GTokenKind.Colon, ":", pos))
        case ',' => ctx.advance(); succeed(GToken(GTokenKind.Comma, ",", pos))
        case _ =>
          ctx.ok = false
          if ctx.index >= ctx.failAt then
            ctx.failAt = ctx.index
            ctx.failMsg = s"unexpected character '$c'"
          fail

@main def profileLexer(): Unit =
  val large =
    """{"data": [""" +
      (1 to 100).map { i =>
        s"""{"id": $i, "name": "item$i", "tags": ["tag${i % 5}", "tag${i % 3}"], "meta": {"created": "2024-01-0${(i % 9) + 1}", "score": ${i * 1.5}}}"""
      }.mkString(", ") +
      """], "status": "ok"}"""

  val iters = 1000

  // Original lexer
  val t0 = System.nanoTime()
  for _ <- 1 to iters do
    GrammaJSONLexer.tokenize(large, GrammaJSONLexer.nextToken)
  val t1 = System.nanoTime()
  println(s"Original lexer:  ${(t1 - t0) / iters / 1000} µs/op")

  // Optimized lexer (first-char dispatch)
  val t2 = System.nanoTime()
  for _ <- 1 to iters do
    OptimizedJSONLexer.tokenize(large, OptimizedJSONLexer.nextToken)
  val t3 = System.nanoTime()
  println(s"Optimized lexer: ${(t3 - t2) / iters / 1000} µs/op")

  // Optimized full pipeline
  val t4 = System.nanoTime()
  for _ <- 1 to iters do
    val tokens = OptimizedJSONLexer.tokenize(large, OptimizedJSONLexer.nextToken).toOption.get
    GrammaJSONParser.parseTokens(tokens)
  val t5 = System.nanoTime()
  println(s"Optimized combined: ${(t5 - t4) / iters / 1000} µs/op")

  // ScalaComb baseline
  val t6 = System.nanoTime()
  for _ <- 1 to iters do
    ScalaCombJSON.parse(large)
  val t7 = System.nanoTime()
  println(s"ScalaComb:          ${(t7 - t6) / iters / 1000} µs/op")
