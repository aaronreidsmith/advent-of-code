package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day07Test extends BaseTest {
  override val runPart1: Boolean = isCI
  override val runPart2: Boolean = isCI

  val suite: Suite = Suite(fileInput, "46065", "14134")
}
