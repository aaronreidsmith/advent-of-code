package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}
import org.scalatest.tagobjects.Slow

class Day18Test extends BaseTest {
  "Day18.part1" should "work on example input" in {
    val input = Day18.parseInput(
      """
        |.#.#...|#.
        |.....#|##|
        |.|..|...#.
        |..|#.....#
        |#.#|||#|#|
        |...#.||...
        |.|....|...
        |||...#|.#|
        ||.||||..|.
        |...#.|..|.
        |""".stripMargin.asSource
    )
    Day18.part1(input) shouldBe 1147
  }

  "Day18.part2" should "work on actual input" taggedAs Slow in {
    val input = using("2018/day18.txt")(Day18.parseInput)
    Day18.part2(input) shouldBe 169106
  }
}
