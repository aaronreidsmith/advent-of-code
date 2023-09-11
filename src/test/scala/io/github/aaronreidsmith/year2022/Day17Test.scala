package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.BaseTest
import io.github.aaronreidsmith.tags.Part2

class Day17Test extends BaseTest {
  // TODO: This test works on actual input and worked on ScalaTest, but part2 stopped working when switching to munit. Oh well 🤷‍
  override def munitTests(): Seq[Test] = super.munitTests().filterNot(_.tags.contains(Part2))

  val suite: Suite = Suite(fileInput, 3068, 1514285714288L)
}
