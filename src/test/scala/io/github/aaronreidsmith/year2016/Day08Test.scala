package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{BaseTest, using}

class Day08Test extends BaseTest {
  override val suite: Suite = Suite(
    fileInput,
    116,
    fileInput,
    // Have to do this because trailing spaces are stripped in multiline strings
    Seq(
      "",
      "#  # ###   ##    ## #### #    ###   ##  #### #### ",
      "#  # #  # #  #    # #    #    #  # #  # #       # ",
      "#  # #  # #  #    # ###  #    ###  #    ###    #  ",
      "#  # ###  #  #    # #    #    #  # #    #     #   ",
      "#  # #    #  # #  # #    #    #  # #  # #    #    ",
      " ##  #     ##   ##  #    #### ###   ##  #### #### ",
      ""
    ).mkString("\n")
  )
}
