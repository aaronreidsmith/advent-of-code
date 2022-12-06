package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day02Test extends BaseTest {
  "Day02.part1" should "work on example input" in {
    val input = Day02.parseInput(
      Source.fromString(
        s"""5\t1\t9\t5
           |7\t5\t3
           |2\t4\t6\t8""".stripMargin
      )
    )
    Day02.part1(input) shouldBe 18
  }

  "Day02.part2" should "work on example input" in {
    val input = Day02.parseInput(
      Source.fromString(
        s"""5\t9\t2\t8
           |9\t4\t7\t3
           |3\t8\t6\t5""".stripMargin
      )
    )
    Day02.part2(input) shouldBe 9
  }
}
