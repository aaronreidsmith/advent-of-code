package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

class Day14Test extends BaseTest {
  "Day14.part1" should "work on example input" in {
    Day14.part1(9) shouldBe "5158916779"
    Day14.part1(5) shouldBe "0124515891"
    Day14.part1(18) shouldBe "9251071085"
    Day14.part1(2018) shouldBe "5941429882"
  }

  "Day14.part2" should "work on example input" in {
    Day14.part2(51589) shouldBe 9
    Day14.part2(92510) shouldBe 18
    Day14.part2(59414) shouldBe 2018
  }
}
