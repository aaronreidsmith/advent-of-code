package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.BaseTest

class Day16Test extends BaseTest {
  val suite: Suite = Suite(
    Seq(
      "8A004A801A8002F478",
      "620080001611562C8802118E34",
      "C0015000016115A2E0802F182340",
      "A0016C880162017C3686B18A3D4780"
    ).parsed,
    Seq(16, 12, 23, 31),
    Seq(
      "C200B40A82",
      "04005AC33890",
      "880086C3E88112",
      "CE00C43D881120",
      "D8005AC2A8F0",
      "F600BC2D8F",
      "9C005AC2F8F0",
      "9C0141080250320F1802104A08"
    ).parsed,
    Seq(3L, 54L, 7L, 9L, 1L, 0L, 0L, 1L)
  )
}
