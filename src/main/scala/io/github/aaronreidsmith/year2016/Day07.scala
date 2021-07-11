package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec
import scala.io.Source

object Day07 {
  def main(args: Array[String]): Unit = {
    val input      = Source.fromResource("2016/day07.txt")
    val inputLines = input.getLines().toList
    input.close()

    val part1 = inputLines.count(supportsTls)
    println(s"Part 1: $part1")
    val part2 = inputLines.count(supportsSsl)
    println(s"Part 2: $part2")
  }

  private def matchesAbba(entry: String): Boolean = entry.sliding(4).exists { quartet =>
    val Array(a, b, c, d, _*) = quartet.split("")
    a == d && b == c && a != b
  }

  @tailrec
  private def split(
      ipAddress: String,
      inBrackets: Boolean = false,
      noBrackets: List[String] = List(),
      brackets: List[String] = List()
  ): (List[String], List[String]) = if (ipAddress.isEmpty) {
    (noBrackets, brackets)
  } else {
    if (inBrackets) {
      val end         = ipAddress.indexOf(']')
      val newBrackets = ipAddress.take(end)
      val remaining   = ipAddress.drop(end + 1)
      split(remaining, inBrackets = false, noBrackets, brackets :+ newBrackets)
    } else {
      val end           = ipAddress.indexOf('[')
      val newNoBrackets = if (end == -1) ipAddress else ipAddress.take(end)
      val remaining     = if (end == -1) "" else ipAddress.drop(end + 1)
      split(remaining, inBrackets = true, noBrackets :+ newNoBrackets, brackets)
    }
  }

  def supportsSsl(ip: String): Boolean = {
    val (noBrackets, inBrackets) = split(ip)
    val abaSequences = noBrackets.flatMap { entry =>
      entry.sliding(3).flatMap { triplet =>
        val Array(a, b, c, _*) = triplet.split("")
        if (a == c && a != b) Some(triplet) else None
      }
    }
    inBrackets.exists { entry =>
      entry.sliding(3).exists { triplet =>
        val Array(a, b, c, _*) = triplet.split("")
        a == c && a != b && abaSequences.contains(s"$b$a$b")
      }
    }
  }

  def supportsTls(ip: String): Boolean = {
    val (noBrackets, inBrackets) = split(ip)
    noBrackets.exists(matchesAbba) && !inBrackets.exists(matchesAbba)
  }
}
