package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day09Test extends BaseTest {
  "Day09.part1" should "work on example input" in {
    Seq(
      ("1102,34915192,34915192,7,4,7,99,0", 1219070632396864L),
      ("104,1125899906842624,99", 1125899906842624L)
    ).foreach {
      case (input, expected) => Day09.part1(Day09.parseInput(input.asSource)) shouldBe expected
    }
  }

  "Day09.part2" should "work on actual input" in {
    val input = using("2019/day09.txt")(Day09.parseInput)
    Day09.part2(input) shouldBe 66113L
  }
}
