package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day04Test extends BaseTest {
  override val runPart1: Boolean = isCI
  override val runPart2: Boolean = isCI

  val suite: Suite = Suite(
    Seq("abcdef", "pqrstuv"),
    Seq(609043, 1048970),
    Seq("abcdef"),
    Seq(6742839)
  )
}
