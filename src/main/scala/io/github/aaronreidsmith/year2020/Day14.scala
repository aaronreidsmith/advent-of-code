package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

object Day14 {
  type I = List[String]
  type O1 = Long
  type O2 = Long

  // Only want to compile these once
  private val mask   = """^mask = (.*)$""".r
  private val memory = """^mem\[(\d+)] = (\d+)$""".r

  def main(args: Array[String]): Unit = {
    val input = using("2020/day14.txt")(_.getLines().toList)

    val (part1Memory, _) = input.foldLeft((Map.empty[Int, Long], "")) {
      case ((currentMem, _), mask(value)) => (currentMem, value)
      case ((currentMem, currentMask), memory(address, value)) =>
        (currentMem.updated(address.toInt, part1(currentMask, value.toInt)), currentMask)
      case (acc, _) => acc
    }
    println(s"Part 1: ${part1Memory.values.sum}")

    val (part2Memory, _) = input.foldLeft((Map.empty[Long, Long], "")) {
      case ((currentMem, _), mask(value)) => (currentMem, value)
      case ((currentMem, currentMask), memory(address, value)) =>
        val updatedMem = part2(currentMask, address.toInt).map(_ -> value.toLong).toMap
        (currentMem ++ updatedMem, currentMask)
      case (acc, _) => acc
    }
    println(s"Part 2: ${part2Memory.values.sum}")
  }

  private def part1(mask: String, num: Int): Long = {
    val masked = zipWithMask(num, mask).map {
      case (maskDigit, digit) => if (maskDigit == 'X') digit else maskDigit
    }.mkString
    java.lang.Long.parseLong(masked, 2)
  }

  private def part2(mask: String, num: Int): Seq[Long] = {
    def findAllMasks(zipped: List[(Char, Char)], prefix: List[Char] = Nil): Seq[Long] = zipped match {
      case Nil => Seq(java.lang.Long.parseLong(prefix.mkString.reverse, 2))
      case (maskDigit, digit) :: tail =>
        maskDigit match {
          case '0' => findAllMasks(tail, digit :: prefix)
          case '1' => findAllMasks(tail, maskDigit :: prefix)
          case _   => Seq(findAllMasks(tail, '0' :: prefix), findAllMasks(tail, '1' :: prefix)).flatten
        }
    }
    findAllMasks(zipWithMask(num, mask).toList)
  }

  private def zipWithMask(num: Int, mask: String): Seq[(Char, Char)] = {
    val binary       = num.toBinaryString
    val paddedBinary = binary.reverse.padTo(mask.length, '0').reverse
    mask.zip(paddedBinary)
  }
}
