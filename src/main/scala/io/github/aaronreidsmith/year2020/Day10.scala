package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

object Day10 {
  def main(args: Array[String]): Unit = {
    val adaptors      = using("2020/day10.txt")(_.getLines().toList.map(_.toInt).sorted)
    val deviceJoltage = adaptors.last + 3
    val joltageList   = 0 :: (adaptors :+ deviceJoltage)
    println(s"Part 1: ${part1(joltageList)}")
  }

  @tailrec
  private def part1(joltageList: List[Int], differences: List[Int] = Nil): Int = joltageList match {
    case _ :: Nil    => differences.count(_ == 1) * differences.count(_ == 3)
    case a :: b :: _ => part1(joltageList.tail, (b - a) :: differences)
  }
}
