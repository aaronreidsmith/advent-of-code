package io.github.aaronreidsmith.year2016

import java.math.BigInteger
import java.security.MessageDigest
import scala.annotation.tailrec

object Day05 {
  private val md = MessageDigest.getInstance("MD5")

  def main(args: Array[String]): Unit = {
    val input = "cxdnnyjw"
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${getId2(input)}")
  }

  @tailrec
  private def part1(doorId: String, index: Int = 0, password: String = ""): String = {
    val (nextChar, interestingIndex) = LazyList
      .from(index)
      .collectFirst {
        case i if md5(s"$doorId$i").startsWith("00000") => (md5(s"$doorId$i").charAt(5), i)
      }
      .getOrElse((' ', -1))

    val newPassword = password + nextChar.toString
    if (newPassword.length >= 8) newPassword else part1(doorId, interestingIndex + 1, newPassword)
  }

  // Adapted from https://github.com/bahuljain/scala-fun/blob/master/src/adventcode2016/Day5.scala
  @tailrec
  def getId2(doorId: String, index: Int = 0, password: Map[Int, Char] = Map()): String = if (password.size == 8) {
    password.toList
      .sortBy { case (i, _) => i }
      .map { case (_, char) => char }
      .mkString
  } else {
    val hash           = md5(doorId + index)
    val insertPosition = hash(5).asDigit
    if (hash.startsWith("00000") && insertPosition >= 0 && insertPosition < 8 && !password.contains(insertPosition))
      getId2(doorId, index + 1, password + (insertPosition -> hash(6)))
    else getId2(doorId, index + 1, password)
  }

  private def md5(input: String): String = {
    val digest = md.digest(input.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hash   = bigInt.toString(16)
    if (hash.length < 32) hash.reverse.padTo(32, '0').reverse else hash
  }
}
