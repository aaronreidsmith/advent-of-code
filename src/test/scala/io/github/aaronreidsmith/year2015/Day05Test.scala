package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.BaseTest

class Day05Test extends BaseTest {
  private val part1Input = List(
    "ugknbfddgicrmopn", // Nice
    "aaa",              // Nice
    "jchzalrnumimnmhp", // Naughty
    "haegwjzuvuyypxyu", // Naughty
    "dvszwmarrgswjxmb"  // Naughty
  )
  private val part2Input = List(
    "qjhvhtzxzqqjkmpb", // Nice
    "xxyxx",            // Nice
    "uurcxstgmygtbstg", // Naughty
    "ieodomkazucvgmuy"  // Naughty
  )
  val suite: Suite = Suite(
    Seq(part1Input),
    Seq(2),
    Seq(part2Input),
    Seq(2)
  )
}
