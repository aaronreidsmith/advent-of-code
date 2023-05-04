package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest
import org.scalatest.tags.Slow

@Slow
class Day07Test extends BaseTest {
  override val suite: Suite = Suite(fileInput, "46065", "14134")
}
