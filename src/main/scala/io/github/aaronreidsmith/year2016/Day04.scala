package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day04 extends Solution(2016, 4) {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  override protected[year2016] def parseInput(file: Source): List[String] = file.getLines().toList

  override protected[year2016] def part1(input: List[String]): Int = input.foldLeft(0) { (acc, line) =>
    val (_, sectorId, _) = filterAndExtract(line)
    acc + sectorId
  }

  override protected[year2016] def part2(input: List[String]): Int = {
    val target = if (isTest) "very encrypted name" else "northpole object storage"
    input
      .foldLeft(Vector.empty[(String, Int)]) { (acc, line) =>
        val (name, sectorId, _) = filterAndExtract(line)
        if (sectorId != 0) {
          val rotatedName = name.map(rotate(_, sectorId).toString).mkString
          acc :+ (rotatedName, sectorId)
        } else {
          acc
        }
      }
      .collectFirst {
        case (decryptedName, sectorId) if decryptedName == target => sectorId
      }
      .getOrElse(-1)
  }

  private def filterAndExtract(doorId: String): (String, Int, String) = {
    val entry = "^([a-z-]+)-([0-9]+)\\[([a-z]+)]$".r
    doorId match {
      case entry(name, sectorId, checksum) =>
        val top5 = name
          .replace("-", "")
          .groupBy(identity)
          .toList
          .sortBy { case (char, occurrences) => (-occurrences.length, char) }
          .take(5)
          .map { case (char, _) => char }
          .mkString
        val filteredId = if (checksum == top5) sectorId.toInt else 0
        (name, filteredId, checksum)
      case _ => throw new IllegalArgumentException
    }
  }

  @tailrec
  private def rotate(char: Char, times: Int, counter: Int = 0): Char = if (counter >= times) {
    char
  } else {
    val newChar = char match {
      case '-' | ' ' => ' '
      case 'z'       => 'a'
      case _         => (char + 1).toChar
    }
    rotate(newChar, times, counter + 1)
  }
}
