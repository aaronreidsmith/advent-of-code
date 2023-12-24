package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.BaseTest

// TODO: This whole class is acting up. When run via intellij the class can't be found. When run via sbt, it returns 0 for part 1
class Day24Test extends BaseTest {
  override def munitTests(): Seq[Test] = Seq()

  val suite: Suite = Suite(
    fileInput,
    16050,
    669042940632377L
  )
}
