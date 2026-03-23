package io.github.edadma.gramma.bench

@main def profile(): Unit =
  val large =
    """{"data": [""" +
      (1 to 100).map { i =>
        s"""{"id": $i, "name": "item$i", "tags": ["tag${i % 5}", "tag${i % 3}"], "meta": {"created": "2024-01-0${(i % 9) + 1}", "score": ${i * 1.5}}}"""
      }.mkString(", ") +
      """], "status": "ok"}"""

  println(s"Input length: ${large.length} chars")

  // Time tokenization separately
  val t0 = System.nanoTime()
  val iters = 1000
  var tokens: Array[GToken] = null
  for _ <- 1 to iters do
    tokens = GrammaJSONLexer.tokenize(large, GrammaJSONLexer.nextToken).toOption.get
  val t1 = System.nanoTime()
  println(s"Token count: ${tokens.length}")
  println(s"Tokenize: ${(t1 - t0) / iters / 1000} µs/op")

  // Time parsing separately (reuse tokens)
  val t2 = System.nanoTime()
  for _ <- 1 to iters do
    GrammaJSONParser.parseTokens(tokens)
  val t3 = System.nanoTime()
  println(s"Parse:    ${(t3 - t2) / iters / 1000} µs/op")

  // Time combined
  val t4 = System.nanoTime()
  for _ <- 1 to iters do
    GrammaJSON.parse(large)
  val t5 = System.nanoTime()
  println(s"Combined: ${(t5 - t4) / iters / 1000} µs/op")

  // Compare with scala-combinators
  val t6 = System.nanoTime()
  for _ <- 1 to iters do
    ScalaCombJSON.parse(large)
  val t7 = System.nanoTime()
  println(s"ScalaComb: ${(t7 - t6) / iters / 1000} µs/op")

  // Show token breakdown
  val kinds = tokens.groupBy(_.kind).view.mapValues(_.length).toMap
  println(s"\nToken kinds: $kinds")
