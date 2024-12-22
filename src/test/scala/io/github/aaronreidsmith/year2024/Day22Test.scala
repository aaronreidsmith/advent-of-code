package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.BaseTest

class Day22Test extends BaseTest {
  val suite: Suite = Suite(
    """1
      |10
      |100
      |2024""".stripMargin.parsed,
    37327623L,
    """1
      |2
      |3
      |2024""".stripMargin.parsed,
    23
  )
}
