package io.github.aaronreidsmith.year2016

import scala.io.Source

object Day15 {
  private val entry = "^Disc #(\\d+) has (\\d+) positions; at time=0, it is at position (\\d+)\\.$".r

  protected[this] case class Disc(number: Int, positions: Int, initialPosition: Int)

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("2016/day15.txt")
    val discs = input.getLines().foldLeft(List.empty[Disc]) { (acc, line) =>
      line match {
        case entry(number, positions, initialPosition) =>
          acc :+ Disc(number.toInt, positions.toInt, initialPosition.toInt)
        case _ => acc
      }
    }
    input.close()

    println(s"Part 1: ${waitTime(discs)}")

    val updatedDiscs = discs :+ Disc(number = discs.length + 1, positions = 11, initialPosition = 0)
    println(s"Part 2: ${waitTime(updatedDiscs)}")
  }

  private def position(disc: Disc, dropTime: Int): Int = (disc.number + disc.initialPosition + dropTime) % disc.positions

  private def waitTime(discs: List[Disc]): Int = LazyList
    .from(0)
    .find(t => discs.forall(disc => position(disc, t) == 0))
    .getOrElse(-1)
}
