package io.github.aaronreidsmith.year2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day05Test extends AnyFlatSpec with Matchers {
  "Day05.part1" should "work on example input" in {
    val words = List(
      "ugknbfddgicrmopn", // Nice
      "aaa",              // Nice
      "jchzalrnumimnmhp", // Naughty
      "haegwjzuvuyypxyu", // Naughty
      "dvszwmarrgswjxmb"  // Naughty
    )
    Day05.part1(words) shouldBe 2
  }

  "Day05.part2" should "work on example input" in {
    val words = List(
      "qjhvhtzxzqqjkmpb", // Nice
      "xxyxx",            // Nice
      "uurcxstgmygtbstg", // Naughty
      "ieodomkazucvgmuy"  // Naughty
    )
    Day05.part2(words) shouldBe 2
  }
}
