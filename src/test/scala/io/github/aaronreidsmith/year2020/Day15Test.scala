package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.BaseTest

class Day15Test extends BaseTest {
  override val runPart1: Boolean = isCI
  override val runPart2: Boolean = isCI

  val suite: Suite = Suite(
    Seq("1,3,2", "2,1,3", "1,2,3", "2,3,1", "3,2,1", "3,1,2").parsed,
    Seq(1, 10, 27, 78, 438, 1836),
    Seq("0,3,6", "1,3,2", "2,1,3", "1,2,3", "2,3,1", "3,2,1", "3,1,2").parsed,
    Seq(175594, 2578, 3544142, 261214, 6895259, 18, 362)
  )
}
