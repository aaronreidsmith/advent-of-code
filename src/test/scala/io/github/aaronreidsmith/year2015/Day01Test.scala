package io.github.aaronreidsmith.year2015

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day01Test extends AnyFlatSpec with Matchers {
  "Day01.part1" should "work on example input" in {
    Day01.part1("(())") shouldBe 0
    Day01.part1("()()") shouldBe 0
    Day01.part1("(((") shouldBe 3
    Day01.part1("(()(()(") shouldBe 3
    Day01.part1("))(((((") shouldBe 3
    Day01.part1("())") shouldBe -1
    Day01.part1("))(") shouldBe -1
    Day01.part1(")))") shouldBe -3
    Day01.part1(")())())") shouldBe -3
  }

  "Day01.part2" should "work on example input" in {
    Day01.part2(")") shouldBe 1
    Day01.part2("()())") shouldBe 5
  }
}
