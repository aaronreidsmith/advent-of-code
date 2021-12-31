package io.github.aaronreidsmith.year2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Test extends AnyFlatSpec with Matchers {
  private val containers = List(
    Day17.Container(20),
    Day17.Container(15),
    Day17.Container(10),
    Day17.Container(5),
    Day17.Container(5),
  )

  "Day17.part1" should "work on example input" in {
    Day17.part1(containers, 25) shouldBe 4
  }

  "Day17.part2" should "work on example input" in {
    Day17.part2(containers, 25) shouldBe 3
  }
}
