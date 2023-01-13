package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import java.math.BigInteger
import java.security.MessageDigest
import scala.io.Source

object Day17 extends Solution {
  type I  = String
  type O1 = String
  type O2 = Int

  override def parseInput(file: Source): String = file.mkString.trim
  override def part1(input: String): String     = getAllPaths(input).minBy(_.length)
  override def part2(input: String): Int        = getAllPaths(input).map(_.length).max

  // Both solutions require the same traversal, so might as well only do it once
  private var allPaths = Seq.empty[String]
  private def getAllPaths(input: String): Seq[String] = {
    if (allPaths.isEmpty || isTest) {
      val open = 'b' to 'z'
      def helper(currentPath: String, position: (Int, Int)): Seq[String] = if (position == (3, 3)) {
        Seq(currentPath)
      } else {
        val Seq(upOpen, downOpen, leftOpen, rightOpen) = md5(input + currentPath).take(4).map(open.contains)

        val (row, col) = position
        val upPaths    = if (upOpen && row - 1 >= 0) helper(currentPath + "U", (row - 1, col)) else Seq()
        val downPaths  = if (downOpen && row + 1 <= 3) helper(currentPath + "D", (row + 1, col)) else Seq()
        val leftPaths  = if (leftOpen && col - 1 >= 0) helper(currentPath + "L", (row, col - 1)) else Seq()
        val rightPaths = if (rightOpen && col + 1 <= 3) helper(currentPath + "R", (row, col + 1)) else Seq()

        upPaths ++ downPaths ++ leftPaths ++ rightPaths
      }

      allPaths = helper("", (0, 0))
    }

    allPaths
  }

  private val md = MessageDigest.getInstance("MD5")
  private def md5(input: String): String = {
    val digest = md.digest(input.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hash   = bigInt.toString(16)
    if (hash.length < 32) hash.reverse.padTo(32, '0').reverse else hash
  }
}
