package io.github.aaronreidsmith.year2016

import java.math.BigInteger
import java.security.MessageDigest

object Day17 {
  private val md   = MessageDigest.getInstance("MD5")
  private val open = 'b' to 'z'

  def main(args: Array[String]): Unit = {
    val input = "vkjiggvb"

    lazy val allPaths = {
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

      helper("", (0, 0))
    }

    println(s"Part 1: ${allPaths.minBy(_.length)}")
    println(s"Part 2: ${allPaths.map(_.length).max}")
  }

  private def md5(input: String): String = {
    val digest = md.digest(input.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hash   = bigInt.toString(16)
    if (hash.length < 32) hash.reverse.padTo(32, '0').reverse else hash
  }
}
