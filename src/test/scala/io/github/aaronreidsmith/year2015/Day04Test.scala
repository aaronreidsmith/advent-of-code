package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day04Test extends BaseTest {
  "Day04.part1" should "work on example input" in {
    Day04.part1("abcdef") shouldBe 609043
    Day04.part1("pqrstuv") shouldBe 1048970
  }

  "Day04.part2" should "work on example input" in {
    Day04.part2("abcdef") shouldBe 6742839
  }
}
