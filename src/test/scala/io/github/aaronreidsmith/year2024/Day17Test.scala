package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.BaseTest

class Day17Test extends BaseTest {
  val suite: Suite = Suite(
    """Register A: 729
      |Register B: 0
      |Register C: 0
      |
      |Program: 0,1,5,4,3,0""".stripMargin.parsed,
    "4,6,3,5,6,3,5,2,1,0",
    """Register A: 2024
      |Register B: 0
      |Register C: 0
      |
      |Program: 0,3,5,4,3,0""".stripMargin.parsed,
    117440
  )
}
