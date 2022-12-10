package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.implicits.StringOps

class Day18Test extends BaseTest {
  "Day18.part1" should "work on example input" in {
    val grid =
      """.#.#.#
        |...##.
        |#....#
        |..#...
        |#.#..#
        |####..""".stripMargin.toGrid
    Day18.part1(grid) shouldBe 4
  }

  "Day18.part2" should "work on example input" in {
    val grid =
      """##.#.#
        |...##.
        |#....#
        |..#...
        |#.#..#
        |####.#""".stripMargin.toGrid
    Day18.part2(grid) shouldBe 17
  }
}
