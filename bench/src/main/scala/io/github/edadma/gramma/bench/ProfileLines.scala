package io.github.edadma.gramma.bench

@main def profileLines(): Unit =
  val singleLine =
    """{"data": [""" +
      (1 to 100).map { i =>
        s"""{"id": $i, "name": "item$i", "tags": ["tag${i % 5}", "tag${i % 3}"], "meta": {"created": "2024-01-0${(i % 9) + 1}", "score": ${i * 1.5}}}"""
      }.mkString(", ") +
      """], "status": "ok"}"""

  val multiLine =
    "{\n  \"data\": [\n" +
      (1 to 100).map { i =>
        s"""    {"id": $i, "name": "item$i", "tags": ["tag${i % 5}", "tag${i % 3}"], "meta": {"created": "2024-01-0${(i % 9) + 1}", "score": ${i * 1.5}}}"""
      }.mkString(",\n") +
      "\n  ],\n  \"status\": \"ok\"\n}"

  println(s"Single line: ${singleLine.length} chars, ${singleLine.count(_ == '\n')} newlines")
  println(s"Multi line:  ${multiLine.length} chars, ${multiLine.count(_ == '\n')} newlines")

  val iters = 1000

  // Single line - gramma
  val t0 = System.nanoTime()
  for _ <- 1 to iters do GrammaJSON.parse(singleLine)
  val t1 = System.nanoTime()
  println(s"\nSingle line:  gramma ${(t1 - t0) / iters / 1000} µs")

  // Multi line - gramma
  val t2 = System.nanoTime()
  for _ <- 1 to iters do GrammaJSON.parse(multiLine)
  val t3 = System.nanoTime()
  println(s"Multi line:   gramma ${(t3 - t2) / iters / 1000} µs")

  // Single line - scala-comb
  val t4 = System.nanoTime()
  for _ <- 1 to iters do ScalaCombJSON.parse(singleLine)
  val t5 = System.nanoTime()
  println(s"\nSingle line:  scala-comb ${(t5 - t4) / iters / 1000} µs")

  // Multi line - scala-comb
  val t6 = System.nanoTime()
  for _ <- 1 to iters do ScalaCombJSON.parse(multiLine)
  val t7 = System.nanoTime()
  println(s"Multi line:   scala-comb ${(t7 - t6) / iters / 1000} µs")
