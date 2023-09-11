package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day20Test extends BaseTest {
  // Slow tests that are also ignored on CI, so just skip them. Comment out if you want to run these
  override def munitTests(): Seq[Test] = Seq()

  val suite: Suite = Suite(fileInput, 3872, 8600)
}
