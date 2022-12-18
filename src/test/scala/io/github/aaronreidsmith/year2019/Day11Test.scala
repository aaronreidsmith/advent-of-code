package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day11Test extends BaseTest {
  private val input = using("2019/day11.txt")(Day11.parseInput)

  "Day11.part1" should "work on actual input" in {
    Day11.part1(input) shouldBe 2883L
  }

  "Day11.part2" should "work on actual input" in {
    // Multiline strings and `stripMargin` removes extra space that is in real output, so we use this
    Day11.part2(input) shouldBe Seq(
      "",
      " #    #### ###   ##  ###  #     ##  ####   ",
      " #    #    #  # #  # #  # #    #  #    #   ",
      " #    ###  #  # #    #  # #    #      #    ",
      " #    #    ###  #    ###  #    # ##  #     ",
      " #    #    #    #  # #    #    #  # #      ",
      " #### #### #     ##  #    ####  ### ####   ",
      ""
    ).mkString("\n")
  }
}
