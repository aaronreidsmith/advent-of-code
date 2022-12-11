package io.github.aaronreidsmith.year2015

import io.circe._
import io.circe.optics.all._
import io.circe.parser.parse
import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day12 extends Solution(2015, 12) {
  type I  = String
  type O1 = Int
  type O2 = Int

  override protected[year2015] def parseInput(file: Source): String = file.mkString.trim
  override protected[year2015] def part1(input: String): Int = {
    val number = "-?\\d+".r
    number.findAllIn(input).foldLeft(0)(_ + _.toInt)
  }
  override protected[year2015] def part2(input: String): Int = {
    def sumValues(values: Seq[Json]): Int = values.foldLeft(0)((acc, json) => acc + helper(json))

    def helper(json: Json): Int = json match {
      case jsonNumber(num)                                                  => num.toInt.getOrElse(0)
      case jsonArray(array)                                                 => sumValues(array)
      case jsonObject(obj) if !obj.values.toSeq.contains(jsonString("red")) => sumValues(obj.values.toSeq)
      case _                                                                => 0
    }

    helper(parse(input).getOrElse(Json.Null))
  }
}
