package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.annotations.Slow
import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

@Slow(part2 = true)
object Day14 extends Solution {
  type I  = Vector[Point]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): Vector[Point] = {
    // The functions below are modified from day 10. There was a bug where not all returned strings were 32 characters,
    // so that has been fixed
    @tailrec
    def knotHash(
        reversalLengths: Vector[Int],
        list: Vector[Int] = (0 until 256).toVector,
        currentPosition: Int = 0,
        skipSize: Int = 0
    ): String = reversalLengths match {
      case Vector() =>
        list
          .grouped(16)
          .map { bits =>
            val hexBit = bits.reduceLeft(_ ^ _).toHexString
            if (hexBit.length < 2) s"00$hexBit".takeRight(2) else hexBit
          }
          .mkString
      case reversalLength +: tail =>
        val leftRotated   = rotateLeft(list, currentPosition)
        val reversedPart  = leftRotated.take(reversalLength).reverse
        val remainingPart = leftRotated.drop(reversalLength)
        val newList       = rotateRight(reversedPart ++: remainingPart, currentPosition)
        knotHash(tail, newList, (currentPosition + reversalLength + skipSize) % 256, skipSize + 1)
      case _ => throw new IllegalArgumentException
    }

    def rotateLeft(list: Vector[Int], i: Int): Vector[Int] = {
      val size = list.size
      list.drop(i % size) ++ list.take(i % size)
    }

    def rotateRight(list: Vector[Int], i: Int): Vector[Int] = {
      val size = list.size
      list.drop(size - (i % size)) ++ list.take(size - (i % size))
    }

    def toBinary(char: Char): String = {
      val binary = BigInt(char.toString, 32).toString(2)
      if (binary.length < 4) s"0000$binary".takeRight(4) else binary
    }

    val input = file.mkString.trim
    (0 until 128).toVector.flatMap { row =>
      val lengths = Vector.fill(64)(s"$input-$row".map(_.toInt).toVector ++ Vector(17, 31, 73, 47, 23)).flatten
      val hash    = knotHash(lengths)
      val binary  = hash.map(toBinary).mkString
      binary.zipWithIndex.foldLeft(Vector.empty[Point]) {
        case (acc, (char, index)) => if (char == '1') acc :+ Point(row, index) else acc
      }
    }
  }

  override def part1(input: Vector[Point]): Int = input.toSet.size

  override def part2(input: Vector[Point]): Int = {
    def fillRegion(square: Point, unusedSquares: Set[Point]): Set[Point] = {
      val neighbors = square.immediateNeighbors.filter(unusedSquares.contains)
      Set(square) ++ neighbors.flatMap(fillRegion(_, unusedSquares - square -- neighbors))
    }

    val used = input.toSet
    used
      .foldLeft(Set.empty[Set[Point]]) {
        case (regions, square) if !regions.exists(_.contains(square)) => regions + fillRegion(square, used)
        case (regions, _)                                             => regions
      }
      .size
  }
}
