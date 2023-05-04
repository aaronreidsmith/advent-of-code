package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day02Test extends BaseTest {
  private val input         = Seq(List(Day02.Box(2, 3, 4)), List(Day02.Box(1, 1, 10)))
  override val suite: Suite = Suite(input, Seq(58, 43), Seq(34, 14))
}
