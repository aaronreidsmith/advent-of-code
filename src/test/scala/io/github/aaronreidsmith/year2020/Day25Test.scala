package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.tags.Part2

class Day25Test extends BaseTest {
  override def munitTests(): Seq[Test] = super.munitTests().filterNot(_.tags.contains(Part2))
  
  val suite: Suite = Suite((5764801L, 17807724L), 14897079L)
}
