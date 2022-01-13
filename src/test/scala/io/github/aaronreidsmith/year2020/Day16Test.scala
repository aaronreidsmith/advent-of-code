package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{BaseTest, using}

import scala.io.Source

class Day16Test extends BaseTest {
  "Day16.part1" should "work on example input" in {
    val input =
      """class: 1-3 or 5-7
        |row: 6-11 or 33-44
        |seat: 13-40 or 45-50
        |
        |your ticket:
        |7,1,14
        |
        |nearby tickets:
        |7,3,47
        |40,4,50
        |55,2,20
        |38,6,12""".stripMargin
    val (trainInfo, myTicket, otherTickets) = Day16.parseInput(Source.fromString(input))
    val allTickets                          = myTicket +: otherTickets
    Day16.part1(trainInfo, allTickets) shouldBe 71
  }

  // No example input for part 2
  "Day16.part2" should "work on actual input" in {
    val (trainInfo, myTicket, otherTickets) = using("2020/day16.txt")(Day16.parseInput)
    Day16.part2(trainInfo, myTicket, otherTickets) shouldBe 2325343130651L
  }
}