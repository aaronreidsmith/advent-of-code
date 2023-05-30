package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import java.math.BigInteger
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

@Slow(part1 = true, part2 = true)
object Day05 extends Solution {
  type I  = String
  type O1 = String
  type O2 = String

  override def parseInput(file: Source): String = file.mkString.trim

  override def part1(doorId: String): String = {
    val password = new StringBuilder
    @tailrec
    def helper(index: Int): String = {
      val (nextChar, interestingIndex) = LazyList
        .from(index)
        .collectFirst {
          case i if md5(s"$doorId$i").startsWith("00000") => (md5(s"$doorId$i").charAt(5), i)
        }
        .getOrElse((' ', -1))

      password.append(nextChar)
      if (password.size >= 8) password.mkString else helper(interestingIndex + 1)
    }

    helper(0)
  }

  // Adapted from https://github.com/bahuljain/scala-fun/blob/master/src/adventcode2016/Day5.scala
  override def part2(doorId: String): String = {
    val password = mutable.Map.empty[Int, Char]
    @tailrec
    def helper(index: Int): String = if (password.size >= 8) {
      password.toList
        .sortBy((i, _) => i)
        .map((_, char) => char)
        .mkString
    } else {
      val hash           = md5(s"$doorId$index")
      val insertPosition = hash(5).asDigit
      if (hash.startsWith("00000") && insertPosition >= 0 && insertPosition < 8 && !password.contains(insertPosition)) {
        password.addOne(insertPosition -> hash(6))
        helper(index + 1)
      } else {
        helper(index + 1)
      }
    }

    helper(0)
  }

  private val md = MessageDigest.getInstance("MD5")
  private def md5(input: String): String = {
    val digest = md.digest(input.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hash   = bigInt.toString(16)
    if (hash.length < 32) hash.reverse.padTo(32, '0').reverse else hash
  }
}
