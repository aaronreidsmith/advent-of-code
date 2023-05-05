package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day18Test extends BaseTest {
  val suite: Suite = Suite(
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
      |""".stripMargin.parsed,
    1147,
    fileInput,
    169106
  )
}
