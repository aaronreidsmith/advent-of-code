package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day06Test extends BaseTest {
  "Day06.part1" should "work on example input" in {
    val input = Source.fromString(
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
        |K)L""".stripMargin
    )
    val orbits = Day06.parseInput(input)
    Day06.part1(orbits) shouldBe 42
  }

  "Day06.part2" should "work on example input" in {
    val input = Source.fromString(
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
        |I)SAN""".stripMargin
    )
    val orbits = Day06.parseInput(input)
    Day06.part2(orbits) shouldBe 4
  }
}
