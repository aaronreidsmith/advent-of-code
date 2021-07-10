package io.github.aaronreidsmith.year2015

import scala.io.Source

object Day24 {
  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2015/day24.txt")
    val gifts = input.getLines().foldLeft(List.empty[Int])((acc, line) => acc :+ line.toInt)
    input.close()

    def numPresents(compartmentSize: Int): Int =
      (1 to gifts.length).find(n => gifts.takeRight(n).sum > compartmentSize).get + 1

    def minQuantumEntanglement(compartmentSize: Int): BigInt = gifts
      .combinations(numPresents(compartmentSize))
      .withFilter(_.sum == compartmentSize)
      .map(_.foldLeft(BigInt(1))(_ * BigInt(_)))
      .min

    println(s"Part 1: ${minQuantumEntanglement(gifts.sum / 3)}")
    println(s"Part 1: ${minQuantumEntanglement(gifts.sum / 4)}")
  }

}
