package io.github.aaronreidsmith.year2017

import scala.annotation.tailrec

object Day14 {
  private type Region = Set[(Int, Int)]

  def main(args: Array[String]): Unit = {
    val input = "wenycdww"
    val (part1, used) = (0 until 128).foldLeft(0, Set.empty[(Int, Int)]) {
      case ((usedCount, gridAcc), row) =>
        val lengths = List.fill(64)(s"$input-$row".map(_.toInt).toList ++: List(17, 31, 73, 47, 23)).flatten
        val hash    = knotHash(lengths)
        val binary  = hash.map(toBinary).mkString
        val rowEntry = binary.zipWithIndex.foldLeft(Set.empty[(Int, Int)]) {
          case (rowAcc, (char, index)) => if (char == '1') rowAcc + ((row, index)) else rowAcc
        }
        (usedCount + rowEntry.size, gridAcc ++ rowEntry)
    }
    println(s"Part 1: $part1")

    val part2 = used
      .foldLeft(Set.empty[Region]) {
        case (regions, square) =>
          if (!regions.exists(_.contains(square))) regions + fillRegion(square, used) else regions
      }
      .size
    println(s"Part 2: $part2")
  }

  private def fillRegion(square: (Int, Int), unusedSquares: Region): Region = {
    val (x, y) = square
    val neighbors = Seq(
      (x - 1, y),
      (x + 1, y),
      (x, y - 1),
      (x, y + 1)
    ).filter(unusedSquares.contains)

    Set(square) ++ neighbors.flatMap(fillRegion(_, unusedSquares - square -- neighbors))
  }

  private def toBinary(char: Char): String = {
    val binary = BigInt(char.toString, 32).toString(2)
    if (binary.length < 4) s"0000$binary".takeRight(4) else binary
  }

  // The functions below are modified from day 10. There was a bug where not all returned strings were 32 characters,
  // so that has been fixed
  @tailrec
  private def knotHash(
      reversalLengths: List[Int],
      list: List[Int] = Range(0, 256).toList,
      currentPosition: Int = 0,
      skipSize: Int = 0
  ): String = reversalLengths match {
    case Nil =>
      list
        .grouped(16)
        .map { bits =>
          val hexBit = bits.reduceLeft(_ ^ _).toHexString
          if (hexBit.length < 2) s"00$hexBit".takeRight(2) else hexBit
        }
        .mkString
    case reversalLength :: tail =>
      val leftRotated   = rotateLeft(list, currentPosition)
      val reversedPart  = leftRotated.take(reversalLength).reverse
      val remainingPart = leftRotated.drop(reversalLength)
      val newList       = rotateRight(reversedPart ++: remainingPart, currentPosition)
      knotHash(tail, newList, (currentPosition + reversalLength + skipSize) % 256, skipSize + 1)
  }

  private def rotateLeft(list: List[Int], i: Int): List[Int] = {
    val size = list.size
    list.drop(i % size) ++: list.take(i % size)
  }

  private def rotateRight(list: List[Int], i: Int): List[Int] = {
    val size = list.size
    list.drop(size - (i % size)) ++: list.take(size - (i % size))
  }
}
