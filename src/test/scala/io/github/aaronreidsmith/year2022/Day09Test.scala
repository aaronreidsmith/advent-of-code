package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

import scala.io.Source

class Day09Test extends BaseTest {
  "Day09.part1" should "work on example input" in {
    val input = Day09.parseInput(
      Source.fromString(
        """R 4
          |U 4
          |L 3
          |D 1
          |R 4
          |D 1
          |L 5
          |R 2""".stripMargin
      )
    )
    Day09.part1(input) shouldBe 13
  }

  "Day09.part2" should "work on example input" in {
    val input = Day09.parseInput(
      Source.fromString(
        """R 5
          |U 8
          |L 8
          |D 3
          |R 17
          |D 10
          |L 25
          |U 20""".stripMargin
      )
    )
    Day09.part2(input) shouldBe 36
  }
}
