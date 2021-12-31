package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.using
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Test extends AnyFlatSpec with Matchers {
  // This question doesn't have example input, so using real input
  private val sues = using("2015/day16.txt")(Day16.parseInput)

  "Day16.part1" should "work on actual input" in {
    Day16.part1(sues) shouldBe 213
  }

  "Day16.part2" should "work on actual input" in {
    Day16.part2(sues) shouldBe 323
  }
}
