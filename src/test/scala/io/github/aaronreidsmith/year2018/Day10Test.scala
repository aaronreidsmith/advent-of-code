package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day10Test extends BaseTest {
  private val input = using("2018/day10.txt")(Day10.parseInput)

  "Day10.part1" should "work on actual input" in {
    Day10.part1(input) shouldBe 10244
  }

  "Day10.part2" should "work on actual input" in {
    Day10.part2(input) shouldBe Seq(
      "",
      "  ##    #    #  ######   ####   #####   #    #  ######  ######",
      " #  #   #    #  #       #    #  #    #  #   #   #       #     ",
      "#    #  #    #  #       #       #    #  #  #    #       #     ",
      "#    #  #    #  #       #       #    #  # #     #       #     ",
      "#    #  ######  #####   #       #####   ##      #####   ##### ",
      "######  #    #  #       #  ###  #  #    ##      #       #     ",
      "#    #  #    #  #       #    #  #   #   # #     #       #     ",
      "#    #  #    #  #       #    #  #   #   #  #    #       #     ",
      "#    #  #    #  #       #   ##  #    #  #   #   #       #     ",
      "#    #  #    #  #        ### #  #    #  #    #  ######  ######",
      ""
    ).mkString("\n")
  }
}
