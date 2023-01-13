package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.BaseTest
import org.scalatest.tagobjects.Slow

class Day15Test extends BaseTest {
  "Day15.part1" should "work on example input" in {
    Seq(
      ("1,3,2", 1),
      ("2,1,3", 10),
      ("1,2,3", 27),
      ("2,3,1", 78),
      ("3,2,1", 438),
      ("3,1,2", 1836)
    ).foreach {
      case (input, expected) => Day15.part1(Day15.parseInput(input.asSource)) shouldBe expected
    }
  }

  "Day15.part2" should "work on example input" taggedAs Slow in {
    Seq(
      ("0,3,6", 175594),
      ("1,3,2", 2578),
      ("2,1,3", 3544142),
      ("1,2,3", 261214),
      ("2,3,1", 6895259),
      ("3,2,1", 18),
      ("3,1,2", 362)
    ).foreach {
      case (input, expected) => Day15.part2(Day15.parseInput(input.asSource)) shouldBe expected
    }
  }
}
