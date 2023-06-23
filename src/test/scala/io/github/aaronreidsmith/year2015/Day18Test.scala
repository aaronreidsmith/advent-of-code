package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.implicits.toGrid

class Day18Test extends BaseTest {
  val suite: Suite = Suite(
    """.#.#.#
      |...##.
      |#....#
      |..#...
      |#.#..#
      |####..""".stripMargin.toGrid,
    4,
    """##.#.#
      |...##.
      |#....#
      |..#...
      |#.#..#
      |####.#""".stripMargin.toGrid,
    17
  )
}
