package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day03Test extends BaseTest {
  "Day03.part1" should "work on example input" in {
    Day03.part1(">") shouldBe 2
    Day03.part1("^>v<") shouldBe 4
    Day03.part1("^v^v^v^v^v") shouldBe 2
  }

  "Day03.part2" should "work on example input" in {
    Day03.part2("^v") shouldBe 3
    Day03.part2("^>v<") shouldBe 3
    Day03.part2("^v^v^v^v^v") shouldBe 11
  }
}
