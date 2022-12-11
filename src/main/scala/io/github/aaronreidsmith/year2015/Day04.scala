package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{Solution, using}

import java.math.BigInteger
import java.security.MessageDigest
import scala.io.Source

object Day04 extends Solution(2015, 4) {
  type I  = String
  type O1 = Int
  type O2 = Int

  override protected[year2015] def parseInput(file: Source): String = file.mkString.trim

  override protected[year2015] def part1(input: String): Int = LazyList
    .from(1)
    .collectFirst {
      case i if md5(s"$input$i").startsWith("00000") => i
    }
    .getOrElse(-1)

  override protected[year2015] def part2(input: String): Int = LazyList
    .from(1)
    .collectFirst {
      case i if md5(s"$input$i").startsWith("000000") => i
    }
    .getOrElse(-1)

  private def md5(input: String): String = {
    val md     = MessageDigest.getInstance("MD5")
    val digest = md.digest(input.getBytes)
    val bigInt = new BigInteger(1, digest)
    val hash   = bigInt.toString(16)
    if (hash.length < 32) hash.reverse.padTo(32, '0').reverse else hash
  }
}
