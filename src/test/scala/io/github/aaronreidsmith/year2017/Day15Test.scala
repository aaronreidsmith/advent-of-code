package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{BaseTest, IgnoreOnCI}
import org.scalatest.tags.Slow

@Slow
@IgnoreOnCI
class Day15Test extends BaseTest {
  val suite: Suite = Suite(fileInput, 588, 285)
}
