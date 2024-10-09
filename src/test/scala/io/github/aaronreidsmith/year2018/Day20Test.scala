package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day20Test extends BaseTest {
  // Slow tests that are also ignored on CI, so just skip them. Comment out if you want to run these
  override val runPart1: Boolean = false
  override val runPart2: Boolean = false

  val suite: Suite = Suite(fileInput, 3872, 8600)
}
