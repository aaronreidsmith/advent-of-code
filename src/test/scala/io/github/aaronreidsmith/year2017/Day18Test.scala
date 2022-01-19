package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

import scala.io.Source

class Day18Test extends BaseTest {
  private implicit class StringOps(string: String) {
    def asInput: Vector[String] = Day18.parseInput(Source.fromString(string))
  }

  "Day18.part1" should "work on example input" in {
    val input =
      """set a 1
        |add a 2
        |mul a a
        |mod a 5
        |snd a
        |set a 0
        |rcv a
        |jgz a -1
        |set a 1
        |jgz a -2""".stripMargin.asInput
    Day18.part1(input) shouldBe 4
  }

  "Day18.part2" should "work on example input" in {
    val input =
      """snd 1
        |snd 2
        |snd p
        |rcv a
        |rcv b
        |rcv c
        |rcv d""".stripMargin.asInput
    Day18.part2(input) shouldBe 3
  }
}
