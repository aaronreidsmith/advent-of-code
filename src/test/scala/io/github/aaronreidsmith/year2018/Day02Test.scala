package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day02Test extends BaseTest {
  private implicit class StringOps(string: String) {
    def asInput: List[String] = Day02.parseInput(Source.fromString(string))
  }

  "Day02.part1" should "work on example input" in {
    val input =
      """abcdef
        |bababc
        |abbcde
        |abcccd
        |aabcdd
        |abcdee
        |ababab""".stripMargin.asInput
    Day02.part1(input) shouldBe 12
  }

  "Day02.part2" should "work on example input" in {
    val input =
      """abcde
        |fghij
        |klmno
        |pqrst
        |fguij
        |axcye
        |wvxyz""".stripMargin.asInput
    Day02.part2(input) shouldBe "fgij"
  }
}
