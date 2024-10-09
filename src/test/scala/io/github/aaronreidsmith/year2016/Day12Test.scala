package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day12Test extends BaseTest {
  override val runPart1: Boolean = isCI
  override val runPart2: Boolean = isCI
  
  val suite: Suite = Suite(fileInput, 318009, 9227663)
}
