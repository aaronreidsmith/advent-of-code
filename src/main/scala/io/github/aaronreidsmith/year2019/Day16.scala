package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.io.Source
import scala.math.Integral.Implicits.*

object Day16 extends Solution {
  type I  = Vector[Int]
  type O1 = String
  type O2 = String

  override def parseInput(file: Source): Vector[Int] = file.mkString.trim.map(_.asDigit).toVector

  override def part1(input: Vector[Int]): String = {
    Iterator.iterate(input)(naiveFft).drop(100).next().take(8).mkString
  }

  // Adapted from https://git.io/J1CzO
  override def part2(input: Vector[Int]): String = {
    val start         = input.take(7).mkString.toInt
    val (whole, part) = (input.length * 10000 - start) /% input.length // https://stackoverflow.com/a/46459182
    val tail          = (input.takeRight(part) ++ Vector.fill(whole)(input).flatten).toBuffer

    for {
      _     <- 1 to 100
      index <- tail.indices.last to 1 by -1
    } {
      tail(index - 1) = (tail(index - 1) + tail(index)) % 10
    }

    tail.take(8).mkString
  }

  private def naiveFft(input: Vector[Int]): Vector[Int] = input.indices.foldLeft(Vector.empty[Int]) { (acc, index) =>
    val pattern         = List(0, 1, 0, -1).flatMap(entry => List.fill(index + 1)(entry))
    val infinitePattern = LazyList.continually(pattern).flatten.drop(1)
    acc :+ input
      .zip(infinitePattern)
      .foldLeft(0) { case (acc, (num, multiplier)) => acc + num * multiplier }
      .toString
      .last
      .asDigit
  }
}
