package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

class Day14Test extends BaseTest {
  "Day14.part1" should "work on example input" in {
    val input = Day14.parseInput(
      """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        |mem[8] = 11
        |mem[7] = 101
        |mem[8] = 0
        |""".stripMargin.asSource
    )
    Day14.part1(input) shouldBe 165L
  }

  "Day14.part2" should "work on example input" in {
    val input = Day14.parseInput(
      """mask = 000000000000000000000000000000X1001X
        |mem[42] = 100
        |mask = 00000000000000000000000000000000X0XX
        |mem[26] = 1
        |""".stripMargin.asSource
    )
    Day14.part2(input) shouldBe 208L
  }
}
