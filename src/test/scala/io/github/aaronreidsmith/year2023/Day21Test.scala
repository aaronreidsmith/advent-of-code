package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.tags.Part2Slow

class Day21Test extends BaseTest {
  override def munitTests(): Seq[Test] = {
    val default = super.munitTests()
    if (isCI) default else default.filterNot(_.tags.contains(Part2Slow))
  }

  val suite: Suite = Suite(
    fileInput,
    3651,
    607334325965751L
  )
}
