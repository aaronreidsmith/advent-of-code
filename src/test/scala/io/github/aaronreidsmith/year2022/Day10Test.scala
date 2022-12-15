package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.{BaseTest, using}

class Day10Test extends BaseTest {
  private val input = using("2022/day10.txt")(Day10.parseInput)

  "Day10" should "work on example input" in {
    Day10.part1(input) shouldBe 13140
    // Example output on website uses periods instead of spaces, but I switched it so I could read the actual output
    // for my given input. Have to use a seq/mkString instead of stripMargin or Scala strips trailing spaces
    Day10.part2(input) shouldBe Seq(
      "",
      "##  ##  ##  ##  ##  ##  ##  ##  ##  ##  ",
      "###   ###   ###   ###   ###   ###   ### ",
      "####    ####    ####    ####    ####    ",
      "#####     #####     #####     #####     ",
      "######      ######      ######      ####",
      "#######       #######       #######     ",
      ""
    ).mkString("\n")
  }
}
