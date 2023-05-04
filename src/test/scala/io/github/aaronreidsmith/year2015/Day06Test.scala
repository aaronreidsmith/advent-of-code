package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day06Test extends BaseTest {
  // Turn on 1M lights, then turn off 1000 lights, then turn off 4 lights
  private val part1Input = Day06.parseInput(
    """turn on 0,0 through 999,999
      |toggle 0,0 through 999,0
      |turn off 499,499 through 500,500""".stripMargin.asSource
  )
  // Increase brightness by 1 then increase brightness by 2M
  private val part2Input = Day06.parseInput(
    """turn on 0,0 through 0,0
      |toggle 0,0 through 999,999""".stripMargin.asSource
  )
  override val suite: Suite = Suite(
    Seq(part1Input),
    Seq(1000000 - 1000 - 4),
    Seq(part2Input),
    Seq(2000001)
  )
}
