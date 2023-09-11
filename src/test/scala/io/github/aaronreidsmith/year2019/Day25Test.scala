package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.tags.Part2

class Day25Test extends BaseTest {
  override def munitTests(): Seq[Test] = super.munitTests().filterNot(_.tags.contains(Part2))
  
  val suite: Suite = Suite(fileInput, 84410376)
}
