package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day02Test extends BaseTest {
  "Day02.part1" should "work on example input" in {
    Day02.part1(List(Day02.Box(2, 3, 4))) shouldBe 58
    Day02.part1(List(Day02.Box(1, 1, 10))) shouldBe 43
  }

  "Day02.part2" should "work on example input" in {
    Day02.part2(List(Day02.Box(2, 3, 4))) shouldBe 34
    Day02.part2(List(Day02.Box(1, 1, 10))) shouldBe 14
  }
}
