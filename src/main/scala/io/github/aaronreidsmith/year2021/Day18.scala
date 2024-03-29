package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.year2021.Day18.Number.Literal

import scala.annotation.tailrec
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object Day18 extends Solution with JavaTokenParsers {
  type I  = Seq[Number]
  type O1 = Int
  type O2 = Int

  enum Number {
    case Literal(val value: Int)
    case Pair(val left: Number, val right: Number)

    private def explode(number: Number): Option[Number] = {
      def helper(num: Number, depth: Int = 0): Option[(Option[Int], Number, Option[Int])] = num match {
        case Literal(_)                                        => None
        case Pair(Literal(left), Literal(right)) if depth >= 4 => Some((Some(left), Literal(0), Some(right)))
        case Pair(left, right) =>
          helper(left, depth + 1)
            .map { (leftAdd, left, rightAdd) =>
              (leftAdd, Pair(left, rightAdd.map(right.addLeft).getOrElse(right)), None)
            }
            .orElse {
              helper(right, depth + 1).map { (leftAdd, right, rightAdd) =>
                (None, Pair(leftAdd.map(left.addRight).getOrElse(left), right), rightAdd)
              }
            }
      }

      helper(number).map(_._2)
    }

    private def split(number: Number): Option[Number] = number match {
      case Literal(value) if value >= 10 =>
        val half = value.toFloat / 2
        Some(Pair(Literal(half.floor.toInt), Literal(half.ceil.toInt)))
      case Literal(_)        => None
      case Pair(left, right) => split(left).map(Pair(_, right)).orElse(split(right).map(Pair(left, _)))
    }

    def reduce: Number = {
      @tailrec
      def helper(number: Number): Number = explode(number) match {
        case Some(num) => helper(num)
        case None =>
          split(number) match {
            case Some(num) => helper(num)
            case None      => number
          }
      }
      helper(this)
    }

    infix def +(that: Number): Number = Pair(this, that).reduce

    def addLeft(addValue: Int): Number = this match {
      case Literal(value)    => Literal(value + addValue)
      case Pair(left, right) => Pair(left.addLeft(addValue), right)
    }

    def addRight(addValue: Int): Number = this match {
      case Literal(value)    => Literal(value + addValue)
      case Pair(left, right) => Pair(left, right.addRight(addValue))
    }

    def magnitude: Int = this match {
      case Literal(value)    => value
      case Pair(left, right) => (3 * left.magnitude) + (2 * right.magnitude)
    }
  }

  override def parseInput(file: Source): Seq[Number] = file.getLines().toSeq.map { line =>
    def number: Parser[Number] = {
      wholeNumber ^^ (_.toInt) ^^ Literal.apply |
        "[" ~> number ~ "," ~ number <~ "]" ^^ { case left ~ _ ~ right => Number.Pair(left, right) }
    }

    parseAll(number, line).get
  }

  override def part1(input: Seq[Number]): Int = input.reduceLeft(_ + _).magnitude

  override def part2(input: Seq[Number]): Int = {
    /* Use this method because order matters and .combinations gives wrong result. Example:
     *
     * List(1, 2, 3).combinations(2).toList == List(List(1, 2), List(1, 3), List(2, 3))
     *
     * The above expression doesn't yield List(2, 1), List(3, 1), or List(3, 2) which are needed to get the right answer here
     */
    var currentMax = Int.MinValue
    for {
      left  <- input
      right <- input
      if left != right
    } {
      currentMax = currentMax.max((left + right).magnitude)
    }
    currentMax
  }
}
