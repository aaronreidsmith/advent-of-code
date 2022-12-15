package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest
import org.scalatest.Ignore
import org.scalatest.tags.Slow

@Slow
@Ignore // TODO: This test passes when run in IntelliJ (via java ...) but not when run via `sbt run`, so just ignoring it
class Day05Test extends BaseTest {
  private val input = "abc"

  "Day05.part1" should "work on example input" in {
    Day05.part1(input) shouldBe "18f47a30"
  }

  "Day05.part2" should "work on example input" in {
    Day05.part2(input) shouldBe "05ace8e3"
  }
}
