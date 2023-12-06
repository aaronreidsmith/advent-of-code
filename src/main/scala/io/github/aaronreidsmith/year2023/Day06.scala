package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day06 extends Solution {
  type I  = String
  type O1 = Long
  type O2 = Long

  extension (s: String) {
    private def asRaces(part2: Boolean): List[Race] = {
      val Array(times, distances, _*) = s.split('\n'): @unchecked
      if (part2) {
        val time     = times.filter(_.isDigit).toLong
        val distance = distances.filter(_.isDigit).toLong
        List(Race(time, distance))
      } else {
        times
          .split("\\s+")
          .zip(distances.split("\\s+"))
          .tail
          .map((time, distance) => Race(time.toLong, distance.toLong))
          .toList
      }
    }
  }

  private case class Race(time: Long, record: Long) {
    def waysToWin: Long = (0L to time).count { buttonHeldTime =>
      val raceTime         = time - buttonHeldTime
      val distanceTraveled = buttonHeldTime * raceTime
      distanceTraveled > record
    }
  }

  override def parseInput(file: Source): String = file.mkString.trim
  override def part1(input: String): Long       = input.asRaces(false).foldLeft(1L)(_ * _.waysToWin)
  override def part2(input: String): Long       = input.asRaces(true).head.waysToWin
}
