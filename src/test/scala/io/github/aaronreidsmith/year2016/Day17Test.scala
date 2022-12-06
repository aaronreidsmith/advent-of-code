package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day17Test extends BaseTest {
  "Day17" should "work on example input" in {
    Seq(
      ("ihgpwlah", "DDRRRD", 370),
      ("kglvqrro", "DDUDRLRRUDRD", 492),
      ("ulqzkmiv", "DRURDRUDDLLDLUURRDULRLDUUDDDRR", 830)
    ).foreach {
      case (input, expected1, expected2) =>
        println(expected1, expected2)
        Day17.part1(input) shouldBe expected1
        Day17.part2(input) shouldBe expected2
    }
  }
}
