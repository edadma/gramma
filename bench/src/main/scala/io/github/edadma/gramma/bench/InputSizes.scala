package io.github.edadma.gramma.bench

@main def inputSizes(): Unit =
  val b = new JSONBenchmark
  for (name, input) <- List("small" -> b.small, "medium" -> b.medium, "large" -> b.large, "largeMl" -> b.largeMl, "huge" -> b.huge, "massive" -> b.massive, "deep" -> b.deep) do
    val tokens = StdJSONLexer.tokenize(input).toOption.get
    println(f"$name%-10s ${input.length}%,7d chars  ${tokens.length}%,6d tokens")
