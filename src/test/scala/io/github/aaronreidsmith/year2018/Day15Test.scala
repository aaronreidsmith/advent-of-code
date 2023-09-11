package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day15Test extends BaseTest {
  // Skip slow tests locally
  override def munitTests(): Seq[Test] = {
    val default = super.munitTests()
    if (isCI) default else Seq()
  }

  val suite: Suite = Suite(fileInput, 231264, 42224)
}
