package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.BaseTest

class Day16Test extends BaseTest {
  "Day16.part1" should "work on example input" in {
    Seq(
      ("8A004A801A8002F478", 16),
      ("620080001611562C8802118E34", 12),
      ("C0015000016115A2E0802F182340", 23),
      ("A0016C880162017C3686B18A3D4780", 31)
    ).foreach {
      case (input, expected) => Day16.part1(Day16.parseInput(input.asSource)) shouldBe expected
    }
  }

  "Day16.part2" should "work on example input" in {
    Seq(
      ("C200B40A82", 3L),
      ("04005AC33890", 54L),
      ("880086C3E88112", 7L),
      ("CE00C43D881120", 9L),
      ("D8005AC2A8F0", 1L),
      ("F600BC2D8F", 0L),
      ("9C005AC2F8F0", 0L),
      ("9C0141080250320F1802104A08", 1L)
    ).foreach {
      case (input, expected) => Day16.part2(Day16.parseInput(input.asSource)) shouldBe expected
    }
  }
}
