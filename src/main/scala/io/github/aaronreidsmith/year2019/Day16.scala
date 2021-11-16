package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.util.FileUtils

import scala.annotation.tailrec
import scala.io.Source
import scala.math.Integral.Implicits._

object Day16 extends FileUtils {
  private val basePattern = List(0, 1, 0, -1)

  def main(args: Array[String]): Unit = {
    val input = using(Source.fromResource("2019/day16.txt"))(_.mkString.map(_.asDigit).toVector)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  @tailrec
  private def part1(input: Vector[Int], iteration: Int = 0): String = if (iteration >= 100) {
    input.take(8).mkString
  } else {
    part1(naiveFft(input), iteration + 1)
  }

  // Adapted from https://git.io/J1CzO
  private def part2(input: Vector[Int]): String = {
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
    val pattern         = basePattern.flatMap(entry => List.fill(index + 1)(entry))
    val infinitePattern = Stream.continually(pattern.toStream).flatten.drop(1)
    acc :+ input
      .zip(infinitePattern)
      .foldLeft(0) { case (acc, (num, multiplier)) => acc + num * multiplier }
      .toString
      .last
      .asDigit
  }
}
