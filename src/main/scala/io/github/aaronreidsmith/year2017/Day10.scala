package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day10 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = String

  override def parseInput(file: Source): String = file.mkString.trim
  override def part1(input: String): Int        = solution(input.split(',').toVector.map(_.toInt))._1
  override def part2(input: String): String = {
    val modified = Vector.fill(64)(input.map(_.toInt).toVector ++ Vector(17, 31, 73, 47, 23)).flatten
    solution(modified)._2
  }

  private def solution(input: Vector[Int]): (Int, String) = {
    def rotateLeft(list: Vector[Int], i: Int): Vector[Int] = {
      val size = list.size
      list.drop(i % size) ++ list.take(i % size)
    }

    def rotateRight(list: Vector[Int], i: Int): Vector[Int] = {
      val size = list.size
      list.drop(size - (i % size)) ++ list.take(size - (i % size))
    }

    @tailrec
    def helper(
        reversalLengths: Vector[Int],
        list: Vector[Int] = (0 until 256).toVector,
        currentPosition: Int = 0,
        skipSize: Int = 0
    ): (Int, String) = reversalLengths match {
      case Vector() => (list.take(2).product, list.grouped(16).map(_.reduceLeft(_ ^ _).toHexString).mkString)
      case reversalLength +: tail =>
        val leftRotated   = rotateLeft(list, currentPosition)
        val reversedPart  = leftRotated.take(reversalLength).reverse
        val remainingPart = leftRotated.drop(reversalLength)
        val newList       = rotateRight(reversedPart ++ remainingPart, currentPosition)
        helper(tail, newList, (currentPosition + reversalLength + skipSize) % 256, skipSize + 1)
      case _ => throw new IllegalArgumentException
    }

    helper(input)
  }
}
