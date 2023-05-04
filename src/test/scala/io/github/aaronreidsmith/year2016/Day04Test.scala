package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.BaseTest

class Day04Test extends BaseTest {
  override val suite: Suite = Suite(
    Seq(
      List(
        "aaaaa-bbb-z-y-x-123[abxyz]",
        "a-b-c-d-e-f-g-h-987[abcde]",
        "not-a-real-room-404[oarel]",
        "totally-real-room-200[decoy]"
      )
    ),
    Seq(1514),
    Seq(List("qzmt-zixmtkozy-ivhz-343[zimth]")),
    Seq(343)
  )
}
