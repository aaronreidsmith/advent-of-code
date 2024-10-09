package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day15Test extends BaseTest {
  override val runPart1: Boolean = isCI
  override val runPart2: Boolean = isCI

  val suite: Suite = Suite(fileInput, 231264, 42224)
}
