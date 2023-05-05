package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, IgnoreOnCI}
import org.scalatest.tags.Slow

@Slow
@IgnoreOnCI
class Day20Test extends BaseTest {
  val suite: Suite = Suite(fileInput, 3872, 8600)
}
