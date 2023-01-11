package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

class Day18Test extends BaseTest {
  "Day18.part1" should "work on example input" in {
    val input = Day18.parseInput(
      """########################
        |#...............b.C.D.f#
        |#.######################
        |#.....@.a.B.c.d.A.e.F.g#
        |########################""".stripMargin.asSource
    )
    Day18.part1(input) shouldBe 132
  }

  "Day18.part2" should "work on example input" in {
    // This is pre-patched in the input, so I had to un-patch it
    val input = Day18.parseInput(
      """#############
        |#g#f.D#..h#l#
        |#F###e#E###.#
        |#dCba...BcIJ#
        |#####.@.#####
        |#nK.L...G...#
        |#M###N#H###.#
        |#o#m..#i#jk.#
        |#############""".stripMargin.asSource
    )
    Day18.part2(input) shouldBe 72
  }
}
