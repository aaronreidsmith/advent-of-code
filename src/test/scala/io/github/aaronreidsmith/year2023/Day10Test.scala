package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.BaseTest

class Day10Test extends BaseTest {
  val suite: Suite = Suite(
    """..F7.
      |.FJ|.
      |SJ.L7
      ||F--J
      |LJ...""".stripMargin.parsed,
    8,
    """.F----7F7F7F7F-7....
      |.|F--7||||||||FJ....
      |.||.FJ||||||||L7....
      |FJL7L7LJLJ||LJ.L-7..
      |L--J.L7...LJS7F-7L7.
      |....F-J..F7FJ|L7L7L7
      |....L7.F7||L7|.L7L7|
      |.....|FJLJ|FJ|F7|.LJ
      |....FJL-7.||.||||...
      |....L---J.LJ.LJLJ...""".stripMargin.parsed,
    8
  )
}
