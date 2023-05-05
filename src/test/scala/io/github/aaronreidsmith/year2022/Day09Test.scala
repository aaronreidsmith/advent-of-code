package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.BaseTest

class Day09Test extends BaseTest {
  val suite: Suite = Suite(
    """R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin.parsed,
    13,
    """R 5
      |U 8
      |L 8
      |D 3
      |R 17
      |D 10
      |L 25
      |U 20""".stripMargin.parsed,
    36
  )
}
