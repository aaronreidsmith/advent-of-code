package io.github.aaronreidsmith

import scala.annotation.tailrec
import scala.collection.SortedMap

object AdventOfCode {
  private val solutions2015: SortedMap[Int, Solution] = {
    import year2015._
    SortedMap(
      1  -> Day01,
      2  -> Day02,
      3  -> Day03,
      4  -> Day04,
      5  -> Day05,
      6  -> Day06,
      7  -> Day07,
      8  -> Day08,
      9  -> Day09,
      10 -> Day10,
      11 -> Day11,
      12 -> Day12,
      13 -> Day13,
      14 -> Day14,
      15 -> Day15,
      16 -> Day16,
      17 -> Day17,
      18 -> Day18,
      19 -> Day19,
      20 -> Day20,
      21 -> Day21,
      22 -> Day22,
      23 -> Day23,
      24 -> Day24,
      25 -> Day25
    )
  }

  private val solutions2016: SortedMap[Int, Solution] = {
    import year2016._
    SortedMap(
      1  -> Day01,
      2  -> Day02,
      3  -> Day03,
      4  -> Day04,
      5  -> Day05,
      6  -> Day06,
      7  -> Day07,
      8  -> Day08,
      9  -> Day09,
      10 -> Day10,
      11 -> Day11,
      12 -> Day12,
      13 -> Day13,
      14 -> Day14,
      15 -> Day15,
      16 -> Day16,
      17 -> Day17,
      18 -> Day18,
      19 -> Day19,
      20 -> Day20,
      21 -> Day21,
      22 -> Day22,
      23 -> Day23,
      24 -> Day24,
      25 -> Day25
    )
  }
  private val solutions2017: SortedMap[Int, Solution] = {
    import year2017._
    SortedMap(
      1  -> Day01,
      2  -> Day02,
      3  -> Day03,
      4  -> Day04,
      5  -> Day05,
      6  -> Day06,
      7  -> Day07,
      8  -> Day08,
      9  -> Day09,
      10 -> Day10,
      11 -> Day11,
      12 -> Day12,
      13 -> Day13,
      14 -> Day14,
      15 -> Day15,
      16 -> Day16,
      17 -> Day17,
      18 -> Day18,
      19 -> Day19,
      20 -> Day20,
      21 -> Day21,
      22 -> Day22,
      23 -> Day23,
      24 -> Day24,
      25 -> Day25
    )
  }
  private val solutions2018: SortedMap[Int, Solution] = {
    import year2018._
    SortedMap(
      1  -> Day01,
      2  -> Day02,
      3  -> Day03,
      4  -> Day04,
      5  -> Day05,
      6  -> Day06,
      7  -> Day07,
      8  -> Day08,
      9  -> Day09,
      10 -> Day10,
      11 -> Day11,
      12 -> Day12,
      13 -> Day13,
      14 -> Day14,
      // TODO
      17 -> Day17,
      18 -> Day18,
      19 -> Day19,
      20 -> Day20,
      21 -> Day21,
      // TODO
      25 -> Day25
    )
  }
  private val solutions2019: SortedMap[Int, Solution] = {
    import year2019._
    SortedMap(
      1 -> Day01,
      2 -> Day02,
      // TODO
    )
  }
  private val solutions2020: SortedMap[Int, Solution] = {
    import year2020._
    SortedMap(
      1 -> Day01,
      2 -> Day02,
      3 -> Day03,
      // TODO
    )
  }
  private val solutions2021: SortedMap[Int, Solution] = SortedMap()
  private val solutions2022: SortedMap[Int, Solution] = {
    import year2022._
    SortedMap(
      1  -> Day01,
      2  -> Day02,
      3  -> Day03,
      4  -> Day04,
      5  -> Day05,
      6  -> Day06,
      7  -> Day07,
      8  -> Day08,
      9  -> Day09,
      10 -> Day10,
      11 -> Day11,
      12 -> Day12,
      13 -> Day13
    )
  }

  private val allSolutions: SortedMap[Int, SortedMap[Int, Solution]] = SortedMap(
    2015 -> solutions2015,
    2016 -> solutions2016,
    2017 -> solutions2017,
    2018 -> solutions2018,
    2019 -> solutions2019,
    2020 -> solutions2020,
    2021 -> solutions2021,
    2022 -> solutions2022
  )

  def main(args: Array[String]): Unit = {
    val parsedArgs = parseArgs(args.toList)
    val yearToRun  = parsedArgs.get("year")
    val dayToRun   = parsedArgs.get("day")
    (yearToRun, dayToRun) match {
      case (Some(year), Some(day)) => allSolutions(year)(day).run()
      case (Some(year), None)      => allSolutions(year).values.foreach(_.run())
      case (None, Some(day))       => allSolutions.values.foreach(year => year(day).run())
      case (None, None)            => allSolutions.values.foreach(_.values.foreach(_.run()))
    }
  }

  @tailrec
  private def parseArgs(args: List[String], parsed: Map[String, Int] = Map()): Map[String, Int] = args match {
    case Nil => parsed
    case "--year" :: year :: tail =>
      val yearInt = year.toInt
      val minYear = 2015
      val maxYear = allSolutions.keys.max
      require(
        minYear <= yearInt && yearInt <= maxYear,
        s"Year must be in the following range: $minYear <= year <= $maxYear"
      )
      parseArgs(tail, parsed.updated("year", yearInt))
    case "--day" :: day :: tail =>
      val dayInt = day.toInt
      require(1 <= dayInt && dayInt <= 25, "Day must be in the following range: 1 <= day <= 25")
      parseArgs(tail, parsed.updated("day", dayInt))
    case other => throw new IllegalArgumentException(s"Unknown argument(s) supplied: $other")
  }
}
