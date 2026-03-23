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
    val s = ScalaCombJSON.parse(input)
    val p = PackratJSON.parse(input)
    println(s"gramma=${g.isRight} scala-comb=${s.isRight} packrat=${p.isRight} | $input")
    if !g.isRight then println(s"  gramma error: ${g.left.get}")
    if !s.isRight then println(s"  scala-comb error: ${s.left.get}")
    if !p.isRight then println(s"  packrat error: ${p.left.get}")
