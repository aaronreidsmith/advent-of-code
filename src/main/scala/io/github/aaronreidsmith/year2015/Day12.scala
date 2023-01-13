package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day12 extends Solution {
  type I  = String
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): String = file.mkString.trim
  override def part1(input: String): Int = {
    val number = """-?\d+""".r
    number.findAllIn(input).foldLeft(0)(_ + _.toInt)
  }
  override def part2(input: String): Int = {
    def sumValues(values: mutable.Buffer[ujson.Value]): Int = values.foldLeft(0)((acc, json) => acc + helper(json))

    def helper(json: ujson.Value): Int = json match {
      case ujson.Num(num)                                                       => num.toInt
      case array: ujson.Arr                                                     => sumValues(array.value)
      case obj: ujson.Obj if !obj.value.values.toSeq.contains(ujson.Str("red")) => sumValues(obj.value.values.toBuffer)
      case _                                                                    => 0
    }

    helper(ujson.read(input))
  }
}
