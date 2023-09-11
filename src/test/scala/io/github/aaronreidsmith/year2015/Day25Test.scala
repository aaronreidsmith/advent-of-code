package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.tags.Part2

class Day25Test extends BaseTest {
  override def munitTests(): Seq[Test] = super.munitTests().filterNot(_.tags.contains(Part2))

  val suite: Suite = Suite((2978, 3083), 2650453L)
}
