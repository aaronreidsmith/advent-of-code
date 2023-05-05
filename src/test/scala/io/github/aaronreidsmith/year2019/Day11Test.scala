package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.BaseTest

class Day11Test extends BaseTest {
  val suite: Suite = Suite(
    fileInput,
    2883L,
    // Multiline strings and `stripMargin` removes extra space that is in real output, so we use this
    Seq(
      "",
      " #    #### ###   ##  ###  #     ##  ####   ",
      " #    #    #  # #  # #  # #    #  #    #   ",
      " #    ###  #  # #    #  # #    #      #    ",
      " #    #    ###  #    ###  #    # ##  #     ",
      " #    #    #    #  # #    #    #  # #      ",
      " #### #### #     ##  #    ####  ### ####   ",
      ""
    ).mkString("\n")
  )
}
