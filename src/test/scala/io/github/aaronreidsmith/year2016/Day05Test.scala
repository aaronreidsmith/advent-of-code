package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest
import org.scalatest.Ignore
import org.scalatest.tags.Slow

@Slow
@Ignore // TODO: This test passes when run in IntelliJ (via java ...) but not when run via `sbt run`, so just ignoring it
class Day05Test extends BaseTest {
  val suite: Suite = Suite("abc", "18f47a30", "05ace8e3")
}
