package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day24 extends Solution {
  type I  = List[Int]
  type O1 = BigInt
  type O2 = BigInt

  def run(): Unit = {
    println("Year 2015, Day 24")
    val gifts = using("2015/day24.txt")(parseInput)
    println(s"Part 1: ${part1(gifts)}")
    println(s"Part 2: ${part2(gifts)}")
    println()
  }

  override protected[year2015] def parseInput(file: Source): List[Int] = file.getLines().toList.map(_.toInt)
  override protected[year2015] def part1(gifts: List[Int]): BigInt     = minQuantumEntanglement(gifts, gifts.sum / 3)
  override protected[year2015] def part2(gifts: List[Int]): BigInt     = minQuantumEntanglement(gifts, gifts.sum / 4)

  private def numPresents(gifts: List[Int], compartmentSize: Int): Int =
    (1 to gifts.length).find(n => gifts.takeRight(n).sum > compartmentSize).fold(0)(_ + 1)

  private def minQuantumEntanglement(gifts: List[Int], compartmentSize: Int): BigInt = gifts
    .combinations(numPresents(gifts, compartmentSize))
    .withFilter(_.sum == compartmentSize)
    .map(_.foldLeft(BigInt(1))(_ * BigInt(_)))
    .min

}
