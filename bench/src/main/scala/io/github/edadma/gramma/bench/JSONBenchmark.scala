package io.github.edadma.gramma.bench

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.SECONDS)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
@State(Scope.Benchmark)
class JSONBenchmark:

  // --- Input data ---

  val small: String = """{"name": "Alice", "age": 30, "active": true}"""

  val medium: String =
    """{"users": [""" +
      (1 to 20).map(i => s"""{"id": $i, "name": "user$i", "email": "user$i@example.com", "active": ${i % 2 == 0}}""").mkString(", ") +
      """], "total": 20, "page": 1}"""

  val large: String =
    """{"data": [""" +
      (1 to 100).map { i =>
        s"""{"id": $i, "name": "item$i", "tags": ["tag${i % 5}", "tag${i % 3}"], "meta": {"created": "2024-01-0${(i % 9) + 1}", "score": ${i * 1.5}}}"""
      }.mkString(", ") +
      """], "status": "ok"}"""

  val largeMl: String =
    "{\n  \"data\": [\n" +
      (1 to 100).map { i =>
        s"""    {"id": $i, "name": "item$i", "tags": ["tag${i % 5}", "tag${i % 3}"], "meta": {"created": "2024-01-0${(i % 9) + 1}", "score": ${i * 1.5}}}"""
      }.mkString(",\n") +
      "\n  ],\n  \"status\": \"ok\"\n}"

  val deep: String =
    val nest = (1 to 20).foldLeft("null": String) { (inner, i) =>
      s"""{"level": $i, "child": $inner}"""
    }
    nest

  // --- Gramma benchmarks (tokenize + parse) ---

  @Benchmark
  def grammaSmall(): Any = GrammaJSON.parse(small)

  @Benchmark
  def grammaMedium(): Any = GrammaJSON.parse(medium)

  @Benchmark
  def grammaLarge(): Any = GrammaJSON.parse(large)

  @Benchmark
  def grammaLargeMl(): Any = GrammaJSON.parse(largeMl)

  @Benchmark
  def grammaDeep(): Any = GrammaJSON.parse(deep)

  // --- Gramma StdLexer benchmarks (ergonomic API) ---

  @Benchmark
  def grammaStdSmall(): Any = GrammaStdJSON.parse(small)

  @Benchmark
  def grammaStdMedium(): Any = GrammaStdJSON.parse(medium)

  @Benchmark
  def grammaStdLarge(): Any = GrammaStdJSON.parse(large)

  @Benchmark
  def grammaStdLargeMl(): Any = GrammaStdJSON.parse(largeMl)

  @Benchmark
  def grammaStdDeep(): Any = GrammaStdJSON.parse(deep)

  // --- Gramma lazy benchmarks (on-demand tokenization) ---

  @Benchmark
  def grammaLazySmall(): Any = GrammaJSON.parseLazy(small)

  @Benchmark
  def grammaLazyMedium(): Any = GrammaJSON.parseLazy(medium)

  @Benchmark
  def grammaLazyLarge(): Any = GrammaJSON.parseLazy(large)

  @Benchmark
  def grammaLazyDeep(): Any = GrammaJSON.parseLazy(deep)

  // --- Scala combinator benchmarks (lex + parse) ---

  @Benchmark
  def scalaCombSmall(): Any = ScalaCombJSON.parse(small)

  @Benchmark
  def scalaCombMedium(): Any = ScalaCombJSON.parse(medium)

  @Benchmark
  def scalaCombLarge(): Any = ScalaCombJSON.parse(large)

  @Benchmark
  def scalaCombLargeMl(): Any = ScalaCombJSON.parse(largeMl)

  @Benchmark
  def scalaCombDeep(): Any = ScalaCombJSON.parse(deep)

  // --- Packrat combinator benchmarks (lex + memoized parse) ---

  @Benchmark
  def packratSmall(): Any = PackratJSON.parse(small)

  @Benchmark
  def packratMedium(): Any = PackratJSON.parse(medium)

  @Benchmark
  def packratLarge(): Any = PackratJSON.parse(large)

  @Benchmark
  def packratDeep(): Any = PackratJSON.parse(deep)
