package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import java.math.BigInteger
import java.security.MessageDigest
import scala.io.Source

object Day14 extends Solution(2016, 14) {
  type I  = String
  type O1 = Int
  type O2 = Int

  override protected[year2016] def parseInput(file: Source): String = file.mkString
  override protected[year2016] def part1(input: String): Int        = solution(input, md5)
  override protected[year2016] def part2(input: String): Int        = solution(input, stretchedMd5)

  private def solution(salt: String, md5Generator: String => String): Int = {
    val md5s = LazyList.from(0).map(i => md5Generator(s"$salt$i"))
    md5s.zipWithIndex
      .collect {
        case (hash, index) if isKey(hash, md5s.slice(index + 1, index + 1001)) => index
      }
      .take(64)
      .last
  }

  private def isKey(hash: String, next1000: LazyList[String]): Boolean = {
    val threeCharSlices = hash
      .sliding(3)
      .collectFirst {
        case slice if slice(0) == slice(1) && slice(1) == slice(2) => slice
      }
    threeCharSlices match {
      case Some(chars) =>
        val charInQuestion = chars.head
        next1000.exists { nextHash =>
          nextHash.sliding(5).count { slice =>
            slice(0) == charInQuestion &&
            slice(0) == slice(1) &&
            slice(1) == slice(2) &&
            slice(2) == slice(3) &&
            slice(3) == slice(4)
          } >= 1
        }
      case None => false
    }
  }

  private val md = MessageDigest.getInstance("MD5")
  private def md5(input: String): String = {
    val digest = md.digest(input.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hash   = bigInt.toString(16)
    if (hash.length < 32) hash.reverse.padTo(32, '0').reverse else hash
  }

  private def stretchedMd5(input: String): String = (0 to 2016).foldLeft(input)((acc, _) => md5(acc))
}
