package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day04Test extends BaseTest {
  "Day04.part1" should "work on example input" in {
    val input = List(
      "aaaaa-bbb-z-y-x-123[abxyz]",
      "a-b-c-d-e-f-g-h-987[abcde]",
      "not-a-real-room-404[oarel]",
      "totally-real-room-200[decoy]"
    )
    Day04.part1(input) shouldBe 1514
  }

  "Day04.part2" should "work on example input" in {
    val input = List("qzmt-zixmtkozy-ivhz-343[zimth]")
    Day04.part2(input) shouldBe 343
  }
}
