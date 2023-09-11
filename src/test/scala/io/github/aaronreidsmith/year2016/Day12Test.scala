package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day12Test extends BaseTest {
  // Skip slow tests locally
  override def munitTests(): Seq[Test] = {
    val default = super.munitTests()
    if (isCI) default else Seq()
  }
  
  val suite: Suite = Suite(fileInput, 318009, 9227663)
}
