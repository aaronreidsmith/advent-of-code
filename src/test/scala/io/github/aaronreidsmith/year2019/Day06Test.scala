package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day06Test extends BaseTest {
  "Day06.part1" should "work on example input" in {
    val input = Day06.parseInput(
      """COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L""".stripMargin.asSource
    )
    Day06.part1(input) shouldBe 42
  }

  "Day06.part2" should "work on example input" in {
    val input = Day06.parseInput(
      """COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L
        |K)YOU
        |I)SAN""".stripMargin.asSource
    )
    Day06.part2(input) shouldBe 4
  }
}
