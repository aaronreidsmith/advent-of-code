package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.BaseTest

class Day06Test extends BaseTest {
  "Day06" should "work on example input" in {
    Seq(
      ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19),
      ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23),
      ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23),
      ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29),
      ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26)
    ).foreach {
      case (input, expected1, expected2) =>
        Day06.part1(input) shouldBe expected1
        Day06.part2(input) shouldBe expected2
    }
  }
}
