package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day07 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[String] = file.getLines().toList
  override def part1(input: List[String]): Int        = input.count(supportsTls)
  override def part2(input: List[String]): Int        = input.count(supportsSsl)

  private def matchesAbba(entry: String): Boolean = entry.sliding(4).exists { quartet =>
    val Array(a, b, c, d, _*) = quartet.split(""): @unchecked
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

  private def supportsSsl(ip: String): Boolean = {
    val (noBrackets, inBrackets) = split(ip)
    val abaSequences = noBrackets.flatMap { entry =>
      entry.sliding(3).flatMap { triplet =>
        val Array(a, b, c, _*) = triplet.split(""): @unchecked
        if (a == c && a != b) Some(triplet) else None
      }
    }
    inBrackets.exists { entry =>
      entry.sliding(3).exists { triplet =>
        val Array(a, b, c, _*) = triplet.split(""): @unchecked
        a == c && a != b && abaSequences.contains(s"$b$a$b")
      }
    }
  }

  private def supportsTls(ip: String): Boolean = {
    val (noBrackets, inBrackets) = split(ip)
    noBrackets.exists(matchesAbba) && !inBrackets.exists(matchesAbba)
  }
}
