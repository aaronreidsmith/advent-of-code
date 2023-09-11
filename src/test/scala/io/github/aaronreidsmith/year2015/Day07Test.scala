package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day07Test extends BaseTest {
  // Skip slow tests locally
  override def munitTests(): Seq[Test] = {
    val default = super.munitTests()
    if (isCI) default else Seq()
  }

  val suite: Suite = Suite(fileInput, "46065", "14134")
}
