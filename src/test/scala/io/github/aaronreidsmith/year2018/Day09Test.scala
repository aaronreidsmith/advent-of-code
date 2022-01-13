package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.{BaseTest, using}

class Day09Test extends BaseTest {
  "Day09.part1" should "work on example input" in {
    Day09.part1(10, 1618) shouldBe 8317L
    Day09.part1(13, 7999) shouldBe 146373L
    Day09.part1(17, 1104) shouldBe 2764L
    Day09.part1(21, 6111) shouldBe 54718L
    Day09.part1(30, 5807) shouldBe 37305L
  }

  "Day09.part2" should "work on actual input" in {
    val (maxPlayers, lastMarble) = using("2018/day09.txt")(Day09.parseInput)
    Day09.part2(maxPlayers, lastMarble) shouldBe 3314195047L
  }
}
