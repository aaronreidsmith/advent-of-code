package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day04Test extends BaseTest {
  // Skip slow tests locally
  override def munitTests(): Seq[Test] = {
    val default = super.munitTests()
    if (isCI) default else Seq()
  }

  val suite: Suite = Suite(
    Seq("abcdef", "pqrstuv"),
    Seq(609043, 1048970),
    Seq("abcdef"),
    Seq(6742839)
  )
}
