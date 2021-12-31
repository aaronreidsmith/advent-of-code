package io.github.aaronreidsmith.year2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day02Test extends AnyFlatSpec with Matchers {
  "Day02.part1" should "work on example input" in {
    Day02.part1(List(Day02.Box(2, 3, 4))) shouldBe 58
    Day02.part1(List(Day02.Box(1, 1, 10))) shouldBe 43
  }

  "Day02.part2" should "work on example input" in {
    Day02.part2(List(Day02.Box(2, 3, 4))) shouldBe 34
    Day02.part2(List(Day02.Box(1, 1, 10))) shouldBe 14
  }
}
