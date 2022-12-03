package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day08Test extends BaseTest {
  private val input = using("2016/day08.txt")(Day08.parseInput)

  "Day08.part1" should "work on actual input" in {
    Day08.part1(input) shouldBe 116
  }

  "Day08.part2" should "work on actual input" in {
    // Have to do this because trailing spaces are stripped in multiline strings
    val expected = Seq(
      "#  # ###   ##    ## #### #    ###   ##  #### #### ",
      "#  # #  # #  #    # #    #    #  # #  # #       # ",
      "#  # #  # #  #    # ###  #    ###  #    ###    #  ",
      "#  # ###  #  #    # #    #    #  # #    #     #   ",
      "#  # #    #  # #  # #    #    #  # #  # #    #    ",
      " ##  #     ##   ##  #    #### ###   ##  #### #### ",
      ""
    ).mkString("\n")

    Day08.part2(input) shouldBe expected
  }
}
