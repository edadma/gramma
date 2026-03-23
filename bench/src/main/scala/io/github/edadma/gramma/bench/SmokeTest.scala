package io.github.edadma.gramma.bench

@main def smokeTest(): Unit =
  val inputs = List(
    """{"name": "Alice", "age": 30, "active": true}""",
    """[1, 2, 3]""",
    """{"nested": {"a": [1, null, "hello"]}}""",
    """null""",
    """"hello world"""",
  )

  for input <- inputs do
    val g = GrammaJSON.parse(input)
    val gl = GrammaJSON.parseLazy(input)
    val s = ScalaCombJSON.parse(input)
    val p = PackratJSON.parse(input)
    println(s"gramma=${g.isRight} lazy=${gl.isRight} scala-comb=${s.isRight} packrat=${p.isRight} | $input")
    if !gl.isRight then println(s"  lazy error: ${gl.left.get}")
