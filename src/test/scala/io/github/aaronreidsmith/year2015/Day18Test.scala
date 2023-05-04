package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.implicits.StringOps

class Day18Test extends BaseTest {
  override val suite: Suite = Suite(
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
