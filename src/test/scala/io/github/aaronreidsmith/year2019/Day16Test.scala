package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

class Day16Test extends BaseTest {
  "Day16.part1" should "work on example input" in {
    Seq(
      ("80871224585914546619083218645595", "24176176"),
      ("19617804207202209144916044189917", "73745418"),
      ("69317163492948606335995924319873", "52432133")
    ).foreach {
      case (input, expected) => Day16.part1(Day16.parseInput(input.asSource)) shouldBe expected
    }
  }

  "Day16.part2" should "work on example input" in {
    Seq(
      ("03036732577212944063491565474664", "84462026"),
      ("02935109699940807407585447034323", "78725270"),
      ("03081770884921959731165446850517", "53553731")
    ).foreach {
      case (input, expected) => Day16.part2(Day16.parseInput(input.asSource)) shouldBe expected
    }
  }
}
