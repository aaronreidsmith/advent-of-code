package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.{BaseTest, using}

class Day08Test extends BaseTest {
  private val input = using("2019/day08.txt")(Day08.parseInput)

  "Day08.part1" should "work on actual input" in {
    Day08.part1(input) shouldBe 1950
  }

  "Day08.part2" should "work on actual input" in {
    // Multiline strings and `stripMargin` removes extra space that is in real output, so we use this
    Day08.part2(input) shouldBe Seq(
      "",
      "#### #  #  ##  #  # #    ",
      "#    # #  #  # #  # #    ",
      "###  ##   #  # #### #    ",
      "#    # #  #### #  # #    ",
      "#    # #  #  # #  # #    ",
      "#    #  # #  # #  # #### "
    ).mkString("\n")
  }
}
