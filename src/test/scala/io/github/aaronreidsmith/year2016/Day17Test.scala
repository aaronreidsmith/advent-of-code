package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day17Test extends BaseTest {
  private val inputAndOutput = Seq(
    ("ihgpwlah", "DDRRRD", 370),
    ("kglvqrro", "DDUDRLRRUDRD", 492),
    ("ulqzkmiv", "DRURDRUDDLLDLUURRDULRLDUUDDDRR", 830)
  )

  "Day17" should "work on example input" in {
    inputAndOutput.foreach {
      case (input, expected1, expected2) =>
        Day17.part1(input) shouldBe expected1
        Day17.part2(input) shouldBe expected2
    }
  }
}
