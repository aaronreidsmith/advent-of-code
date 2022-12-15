package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day09Test extends BaseTest {
  "Day09.part1" should "work on example input" in {
    Seq(
      ("{}", 1),
      ("{{{}}}", 6),
      ("{{},{}}", 5),
      ("{{{},{},{{}}}}", 16),
      ("{<a>,<a>,<a>,<a>}", 1),
      ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9),
      ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9),
      ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)
    ).foreach {
      case (input, expected) => Day09.part1(input) shouldBe expected
    }
  }

  "Day09.part2" should "work on example input" in {
    Seq(
      ("<>", 0),
      ("<random characters>", 17),
      ("<<<<>", 3),
      ("<{!>}>", 2),
      ("<!!>", 0),
      ("<!!!>>", 0),
      ("""<{o"i!a,<{i<a>"""", 10)
    ).foreach {
      case (input, expected) => Day09.part2(input) shouldBe expected
    }
  }
}
