package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day02Test extends BaseTest {
  "Day02.part1" should "work on example input" in {
    val input = Day02.parseInput(
      """abcdef
        |bababc
        |abbcde
        |abcccd
        |aabcdd
        |abcdee
        |ababab""".stripMargin.asSource
    )
    Day02.part1(input) shouldBe 12
  }

  "Day02.part2" should "work on example input" in {
    val input = Day02.parseInput(
      """abcde
        |fghij
        |klmno
        |pqrst
        |fguij
        |axcye
        |wvxyz""".stripMargin.asSource
    )
    Day02.part2(input) shouldBe "fgij"
  }
}
