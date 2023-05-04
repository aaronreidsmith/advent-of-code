package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest
import org.scalatest.tags.Slow

@Slow
class Day04Test extends BaseTest {
  override val suite: Suite = Suite(
    Seq("abcdef", "pqrstuv"),
    Seq(609043, 1048970),
    Seq("abcdef"),
    Seq(6742839)
  )
}
