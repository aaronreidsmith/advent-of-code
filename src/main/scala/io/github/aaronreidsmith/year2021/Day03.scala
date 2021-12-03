package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day03 {
  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day03.txt"))(_.getLines().toVector)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private def part1(input: Vector[String]): Int = {
    val gammaRate = input.transpose
      .foldLeft(new StringBuilder) { (acc, column) =>
        val oneCount = column.count(_ == '1')
        val nextChar = if (oneCount > column.length / 2) '1' else '0'
        acc.append(nextChar)
      }
      .mkString
    val epsilonRate = gammaRate.map {
      case '0' => '1'
      case _   => '0'
    }
    Integer.parseInt(gammaRate, 2) * Integer.parseInt(epsilonRate, 2)
  }

  private def part2(input: Vector[String]): Int = {
    @tailrec
    def helper(transposedInput: Vector[Vector[Char]], oxygenGenerator: Boolean, pointer: Int = 0): Int = {
      if (transposedInput.forall(_.length == 1)) {
        Integer.parseInt(transposedInput.flatten.mkString, 2)
      } else {
        val column    = transposedInput(pointer)
        val oneCount  = column.count(_ == '1')
        val zeroCount = column.count(_ == '0')
        val charToFilter = if (oxygenGenerator) {
          if (oneCount == zeroCount || oneCount > column.length / 2) '0' else '1'
        } else {
          if (oneCount == zeroCount || oneCount > column.length / 2) '1' else '0'
        }
        val filtered = transposedInput.transpose.filterNot(row => row(pointer) == charToFilter).transpose
        helper(filtered, oxygenGenerator, pointer + 1)
      }
    }

    val transposed = input.transpose
    helper(transposed, oxygenGenerator = true) * helper(transposed, oxygenGenerator = false)
  }
}
