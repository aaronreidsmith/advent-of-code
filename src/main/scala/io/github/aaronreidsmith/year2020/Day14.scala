package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day14 extends Solution {
  type I  = List[String]
  type O1 = Long
  type O2 = Long

  // Only want to compile these once
  private val mask   = """^mask = (.*)$""".r
  private val memory = """^mem\[(\d+)] = (\d+)$""".r

  override def parseInput(file: Source): List[String] = file.getLines().toList

  override def part1(input: List[String]): Long = {
    def helper(mask: String, num: Int): Long = {
      val masked = zipWithMask(num, mask).map {
        case (maskDigit, digit) => if (maskDigit == 'X') digit else maskDigit
      }.mkString
      java.lang.Long.parseLong(masked, 2)
    }

    input
      .foldLeft((Map.empty[Int, Long], "")) {
        case ((currentMem, _), mask(value)) => (currentMem, value)
        case ((currentMem, currentMask), memory(address, value)) =>
          (currentMem.updated(address.toInt, helper(currentMask, value.toInt)), currentMask)
        case (acc, _) => acc
      }
      ._1
      .values
      .sum
  }

  override def part2(input: List[String]): Long = {
    def helper(mask: String, num: Int): Seq[Long] = {
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

    input
      .foldLeft((Map.empty[Long, Long], "")) {
        case ((currentMem, _), mask(value)) => (currentMem, value)
        case ((currentMem, currentMask), memory(address, value)) =>
          val updatedMem = helper(currentMask, address.toInt).map(_ -> value.toLong).toMap
          (currentMem ++ updatedMem, currentMask)
        case (acc, _) => acc
      }
      ._1
      .values
      .sum
  }

  private def zipWithMask(num: Int, mask: String): Seq[(Char, Char)] = {
    val binary       = num.toBinaryString
    val paddedBinary = binary.reverse.padTo(mask.length, '0').reverse
    mask.zip(paddedBinary)
  }
}
