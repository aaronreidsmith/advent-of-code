package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day04Test extends BaseTest {
  "Day04.part1" should "work on example input" in {
    val input = Day04.parseInput(
      """aa bb cc dd ee
        |aa bb cc dd aa
        |aa bb cc dd aaa""".stripMargin.asSource
    )
    Day04.part1(input) shouldBe 2
  }

  "Day04.part2" should "work on example input" in {
    val input = Day04.parseInput(
      """abcde fghij
        |abcde xyz ecdab
        |a ab abc abd abf abj
        |iiii oiii ooii oooi oooo
        |oiii ioii iioi iiio""".stripMargin.asSource
    )
    Day04.part2(input) shouldBe 3
  }
}
