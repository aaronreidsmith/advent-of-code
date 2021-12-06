package io.github.aaronreidsmith.year2021

import scala.io.Source
import scala.util.Using

object Day06 {
  def main(args: Array[String]): Unit = {
    val input        = Using.resource(Source.fromResource("2021/day06.txt"))(_.mkString.split(',').map(_.toLong))
    val emptyFishMap = (0L to 8L).map(_ -> 0L).toMap
    val fishMap      = input.foldLeft(emptyFishMap)((acc, n) => acc.updated(n, acc(n) + 1))

    def nextGeneration(fish: Map[Long, Long]): Map[Long, Long] = {
      val rotated      = fish.map { case (k, v) => k - 1 -> v }
      val updatedItems = Map(-1L -> 0L, 6L -> (rotated(6) + rotated(-1)), 8L -> rotated(-1))
      rotated ++ updatedItems
    }

    def nthGeneration(n: Int): Map[Long, Long] = (0 until n).foldLeft(fishMap)((acc, _) => nextGeneration(acc))

    println(s"Part 1: ${nthGeneration(80).values.sum}")
    println(s"Part 1: ${nthGeneration(256).values.sum}")
  }
}
