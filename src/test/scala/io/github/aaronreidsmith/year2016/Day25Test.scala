package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.tags.Part2

class Day25Test extends BaseTest {
  // Skip slow tests locally
  override def munitTests(): Seq[Test] = {
    val default = super.munitTests().filterNot(_.tags.contains(Part2))
    if (isCI) default else Seq()
  }

  val suite: Suite = Suite(fileInput, 158)
}
