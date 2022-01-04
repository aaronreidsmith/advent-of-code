package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{BaseTest, using}

class Day09Test extends BaseTest {
  private val graph = using("2015/day09.txt")(Day09.parseInput)

  "Day09.part1" should "work on example input" in {
    Day09.part1(graph) shouldBe 605
  }

  "Day09.part2" should "work on example input" in {
    Day09.part2(graph) shouldBe 982
  }
}
