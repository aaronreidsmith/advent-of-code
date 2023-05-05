package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

class Day18Test extends BaseTest {
  val suite: Suite = Suite(
    """########################
      |#...............b.C.D.f#
      |#.######################
      |#.....@.a.B.c.d.A.e.F.g#
      |########################""".stripMargin.parsed,
    132,
    // This is pre-patched in the input, so I had to un-patch it
    """#############
      |#g#f.D#..h#l#
      |#F###e#E###.#
      |#dCba...BcIJ#
      |#####.@.#####
      |#nK.L...G...#
      |#M###N#H###.#
      |#o#m..#i#jk.#
      |#############""".stripMargin.parsed,
    72
  )
}
