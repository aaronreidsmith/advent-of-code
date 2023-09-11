package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day14Test extends BaseTest {
  // TODO: This works when run in IntelliJ, but not via sbt. Figure that out...
  override def munitTests(): Seq[Test] = Seq()
  
  val suite: Suite = Suite("abc", 22728, 22551)
}
