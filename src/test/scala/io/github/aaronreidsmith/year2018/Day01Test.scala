package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day01Test extends BaseTest {
  private implicit class StringOps(string: String) {
    def asInput: Vector[Int] = Day01.parseInput(Source.fromString(string))
  }

  "Day01.part1" should "work on example input" in {
    val inputs = Seq(
      ("+1\n+1\n+1", 3),
      ("+1\n+1\n-2", 0),
      ("-1\n-2\n-3", -6)
    )
    inputs.foreach {
      case (rawInput, expected) =>
        val numbers = rawInput.asInput
        Day01.part1(numbers) shouldBe expected
    }
  }

  "Day01.part2" should "work on example input" in {
    val inputs = Seq(
      ("+1\n-1", 0),
      ("+3\n+3\n+4\n-2\n-4", 10),
      ("-6\n+3\n+8\n+5\n-6", 5),
      ("+7\n+7\n-2\n-7\n-4", 14)
    )
    inputs.foreach {
      case (rawInput, expected) =>
        val numbers = rawInput.asInput
        Day01.part2(numbers) shouldBe expected
    }
  }
}
