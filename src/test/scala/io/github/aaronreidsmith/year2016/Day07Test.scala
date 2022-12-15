package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day07Test extends BaseTest {
  "Day07.part1" should "work on example input" in {
    val input = List(
      "abba[mnop]qrst",
      "abcd[bddb]xyyx",
      "aaaa[qwer]tyui",
      "ioxxoj[asdfgh]zxcvbn"
    )
    Day07.part1(input) shouldBe 2
  }

  "Day07.part2" should "work on example input" in {
    val input = List(
      "aba[bab]xyz",
      "xyx[xyx]xyx",
      "aaa[kek]eke",
      "zazbz[bzb]cdb"
    )
    Day07.part2(input) shouldBe 3
  }
}
