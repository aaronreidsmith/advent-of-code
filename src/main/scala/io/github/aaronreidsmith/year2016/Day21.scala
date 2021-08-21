package io.github.aaronreidsmith.year2016

import scala.io.Source

object Day21 {
  private val swapPosition    = "^swap position (\\d+) with position (\\d+)$".r
  private val swapLetter      = "^swap letter (.*) with letter (.*)$".r
  private val rotate          = "^rotate (.*) (\\d+) steps?$".r
  private val rotateWithBasis = "^rotate based on position of letter (.*)$".r
  private val reverse         = "^reverse positions (\\d+) through (\\d+)$".r
  private val move            = "^move position (\\d+) to position (\\d+)$".r

  def main(args: Array[String]): Unit = {
    val originalString = "abcdefgh"
    val input          = Source.fromResource("2016/day21.txt")
    val operations     = input.getLines().toList
    input.close()

    def scramble(original: String): String = operations.foldLeft(original) { (acc, operation) =>
      operation match {
        case swapPosition(x, y) =>
          val a = acc.charAt(x.toInt)
          val b = acc.charAt(y.toInt)
          acc.replace(a, '_').replace(b, a).replace('_', b)
        case swapLetter(x, y)                                 => acc.replace(x, "_").replace(y, x).replace("_", y)
        case rotate(direction, steps) if direction == "left"  => rotateLeft(acc, steps.toInt)
        case rotate(direction, steps) if direction == "right" => rotateRight(acc, steps.toInt)
        case rotateWithBasis(letter) =>
          val index = acc.indexOf(letter)
          val steps = if (index >= 4) index + 1 else index
          rotateRight(acc, steps + 1)
        case reverse(x, y) =>
          val (start, end) = (x.toInt, y.toInt)
          val section      = acc.substring(start, end + 1)
          acc.take(start) + section.reverse + acc.drop(end + 1)
        case move(x, y) =>
          val letter   = acc.charAt(x.toInt)
          val removed  = acc.replace(letter.toString, "")
          val newIndex = y.toInt
          removed.take(newIndex) + letter + removed.drop(newIndex)
        case other => throw new IllegalArgumentException(s"'$other' is not a supported operation")
      }
    }

    println(s"Part 1: ${scramble(originalString)}")
    println(s"Part 2: ${originalString.permutations.find(in => scramble(in) == "fbgdceah").get}")
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
