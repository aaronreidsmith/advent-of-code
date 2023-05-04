package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day07Test extends BaseTest {
  override val suite: Suite = Suite(
    Seq(
      List(
        "abba[mnop]qrst",
        "abcd[bddb]xyyx",
        "aaaa[qwer]tyui",
        "ioxxoj[asdfgh]zxcvbn"
      )
    ),
    Seq(2),
    Seq(
      List(
        "aba[bab]xyz",
        "xyx[xyx]xyx",
        "aaa[kek]eke",
        "zazbz[bzb]cdb"
      )
    ),
    Seq(3)
  )
}
