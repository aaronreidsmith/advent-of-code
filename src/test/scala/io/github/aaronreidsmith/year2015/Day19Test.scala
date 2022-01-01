package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

import scala.io.Source

class Day19Test extends BaseTest {
  "Day19.part1" should "work on example input" in {
    val input =
      """H => HO
        |H => OH
        |O => HH
        |
        |HOH""".stripMargin
    val (rules, molecule) = Day19.parseInput(Source.fromString(input))
    Day19.part1(molecule, rules) shouldBe 4
  }

  // Part 2 doesn't work on the sample input for some reason
  "Day19.part2" should "work on actual input" in {
    val (rules, molecule) = using("2015/day19.txt")(Day19.parseInput)
    Day19.part2(molecule, rules) shouldBe 195
  }
}
