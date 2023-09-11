package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day15Test extends BaseTest {
  // Slow tests that are also ignored on CI, so always skip (comment out if you want to run)
  override def munitTests(): Seq[Test] = Seq()

  val suite: Suite = Suite(fileInput, 588, 285)
}
