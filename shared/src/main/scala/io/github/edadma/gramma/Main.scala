package io.github.edadma.gramma

// --- JSON AST for benchmark ---

private sealed trait JValue
private case class JObject(fields: List[(String, JValue)]) extends JValue
private case class JArray(elements: List[JValue]) extends JValue
private case class JString(value: String) extends JValue
private case class JNumber(value: Double) extends JValue
private case class JBool(value: Boolean) extends JValue
private case object JNull extends JValue

// --- JSON grammar ---

private object BenchLexer extends StdLexer:
  delimiters ++= List("{", "}", "[", "]", ":", ",")
  reserved ++= List("true", "false", "null")

private object BenchParser extends StdParsers(BenchLexer):
  def value(using ctx: ParseCtx): P[JValue] =
    obj | arr | stringLit ^^ (JString(_)) |
      numericLit ^^ (n => JNumber(n.toDouble)) |
      keyword("true") ^^ (_ => JBool(true)) |
      keyword("false") ^^ (_ => JBool(false)) |
      keyword("null") ^^ (_ => JNull)

  def arr(using ctx: ParseCtx): P[JValue] =
    delimiter("[") ~> repsep(value, delimiter(",")) <~ delimiter("]") ^^ (JArray(_))

  def obj(using ctx: ParseCtx): P[JValue] =
    delimiter("{") ~> repsep(field, delimiter(",")) <~ delimiter("}") ^^ (JObject(_))

  def field(using ctx: ParseCtx): P[(String, JValue)] =
    stringLit ~ (delimiter(":") ~> value) ^^ { case k ~ v => (k, v) }

// --- Benchmark runner ---

private def bench(name: String, input: String, warmup: Int, iters: Int)(f: String => Any): Unit =
  // warmup
  var i = 0
  while i < warmup do
    f(input)
    i += 1
  // measure
  val t0 = System.currentTimeMillis()
  i = 0
  while i < iters do
    f(input)
    i += 1
  val elapsed = System.currentTimeMillis() - t0
  val usPerOp = if elapsed > 0 then (elapsed * 1000.0) / iters else 0.0
  val opsPerSec = if elapsed > 0 then (iters * 1000.0) / elapsed else 0.0
  println(f"  $name%-12s ${usPerOp}%8.1f µs/op  ${opsPerSec}%12.0f ops/s")

@main def run(args: String*): Unit =
  val small = """{"name": "Alice", "age": 30, "active": true}"""

  val medium =
    """{"users": [""" +
      (1 to 20).map(i => s"""{"id": $i, "name": "user$i", "email": "user$i@example.com", "active": ${i % 2 == 0}}""").mkString(", ") +
      """], "total": 20, "page": 1}"""

  val large =
    "{\n  \"data\": [\n" +
      (1 to 100).map { i =>
        s"""    {"id": $i, "name": "item$i", "tags": ["tag${i % 5}", "tag${i % 3}"], "meta": {"created": "2024-01-0${(i % 9) + 1}", "score": ${i * 1.5}}}"""
      }.mkString(",\n") +
      "\n  ],\n  \"status\": \"ok\"\n}"

  println(s"gramma benchmark ($platform)")
  println(s"  small:  ${small.length} chars")
  println(s"  medium: ${medium.length} chars")
  println(s"  large:  ${large.length} chars")
  println()

  val warmup = 1000
  val iters = 5000

  // Verify correctness first
  assert(BenchParser.parseSource(small)(BenchParser.value).isRight, "small parse failed")
  assert(BenchParser.parseSource(medium)(BenchParser.value).isRight, "medium parse failed")
  assert(BenchParser.parseSource(large)(BenchParser.value).isRight, "large parse failed")

  println("Small:")
  bench("gramma", small, warmup, iters * 10)(s => BenchParser.parseSource(s)(BenchParser.value))

  println("Medium:")
  bench("gramma", medium, warmup, iters)(s => BenchParser.parseSource(s)(BenchParser.value))

  println("Large:")
  bench("gramma", large, warmup, iters)(s => BenchParser.parseSource(s)(BenchParser.value))
