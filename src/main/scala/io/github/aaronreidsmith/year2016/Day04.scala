package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec
import scala.io.Source

object Day04 {
  private val entry = "^([a-z-]+)-([0-9]+)\\[([a-z]+)]$".r("name", "sectorId", "checksum")

  def main(args: Array[String]): Unit = {
    val input      = Source.fromResource("2016/day04.txt")
    val inputLines = input.getLines().toList
    input.close()

    val part1 = inputLines.foldLeft(0) { (acc, line) =>
      val (_, sectorId, _) = filterAndExtract(line)
      acc + sectorId
    }
    println(s"Part 1: $part1")

    val part2 = inputLines
      .foldLeft(List.empty[(String, Int)]) { (acc, line) =>
        val (name, sectorId, _) = filterAndExtract(line)
        if (sectorId != 0) {
          val rotatedName = name.foldLeft("")(_ + rotate(_, sectorId).toString)
          acc :+ (rotatedName, sectorId)
        } else {
          acc
        }
      }
      .collectFirst {
        case (decryptedName, sectorId) if decryptedName == "northpole object storage" => sectorId
      }
      .getOrElse(-1)
    println(s"Part 2: $part2")
  }

  private def filterAndExtract(doorId: String): (String, Int, String) = doorId match {
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
