package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest
import org.scalatest.Ignore
import org.scalatest.tags.Slow

@Slow
@Ignore // TODO: This works when run in IntelliJ, but not via sbt. Figure that out...
class Day14Test extends BaseTest {
  val suite: Suite = Suite("abc", 22728, 22551)
}
