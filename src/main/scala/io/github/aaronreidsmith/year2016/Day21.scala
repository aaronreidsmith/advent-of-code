package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Solution, using}

import scala.io.Source

object Day21 extends Solution {
  type I  = List[String]
  type O1 = String
  type O2 = String

  def run(): Unit = {
    println("Year 2016, Day 21")
    val input = using("2016/day21.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def parseInput(file: Source): List[String] = file.getLines().toList
  override protected[year2016] def part1(input: List[String]): String     = scramble("abcdefgh", input)
  override protected[year2016] def part2(input: List[String]): String = {
    "abcdefgh".permutations.find(scramble(_, input) == "fbgdceah").get
  }

  // Define these top-level so we only have to compile once
  private val swapPosition    = "^swap position (\\d+) with position (\\d+)$".r
  private val swapLetter      = "^swap letter (.*) with letter (.*)$".r
  private val rotate          = "^rotate (.*) (\\d+) steps?$".r
  private val rotateWithBasis = "^rotate based on position of letter (.*)$".r
  private val reverse         = "^reverse positions (\\d+) through (\\d+)$".r
  private val move            = "^move position (\\d+) to position (\\d+)$".r
  private def scramble(original: String, operations: List[String]): String = operations.foldLeft(original) {
    case (acc, swapPosition(x, y)) =>
      val a = acc.charAt(x.toInt)
      val b = acc.charAt(y.toInt)
      acc.replace(a, '_').replace(b, a).replace('_', b)
    case (acc, swapLetter(x, y))                                 => acc.replace(x, "_").replace(y, x).replace("_", y)
    case (acc, rotate(direction, steps)) if direction == "left"  => rotateLeft(acc, steps.toInt)
    case (acc, rotate(direction, steps)) if direction == "right" => rotateRight(acc, steps.toInt)
    case (acc, rotateWithBasis(letter)) =>
      val index = acc.indexOf(letter)
      val steps = if (index >= 4) index + 1 else index
      rotateRight(acc, steps + 1)
    case (acc, reverse(x, y)) =>
      val (start, end) = (x.toInt, y.toInt)
      val section      = acc.substring(start, end + 1)
      acc.take(start) + section.reverse + acc.drop(end + 1)
    case (acc, move(x, y)) =>
      val letter   = acc.charAt(x.toInt)
      val removed  = acc.replace(letter.toString, "")
      val newIndex = y.toInt
      removed.take(newIndex) + letter + removed.drop(newIndex)
    case (acc, _) => acc
  }

  private def rotateLeft(input: String, i: Int): String = {
    val size = input.length
    input.drop(i % size) ++: input.take(i % size)
  }

  private def rotateRight(input: String, i: Int): String = {
    val size = input.length
    input.drop(size - (i % size)) ++: input.take(size - (i % size))
  }
}
