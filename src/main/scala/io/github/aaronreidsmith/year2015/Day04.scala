package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import java.math.BigInteger
import java.security.MessageDigest

object Day04 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2015, Day 4")
    val input = "yzbqklnj"
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2015] def part1(input: String): Int = LazyList
    .from(1)
    .collectFirst {
      case i if md5(s"$input$i").startsWith("00000") => i
    }
    .getOrElse(-1)

  override protected[year2015] def part2(input: String): Int = LazyList
    .from(9_000_000)
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
