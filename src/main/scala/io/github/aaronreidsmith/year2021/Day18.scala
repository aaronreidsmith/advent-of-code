package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.util.parsing.combinator.JavaTokenParsers

object Day18 extends JavaTokenParsers {
  private sealed trait Number {
    private def explode(number: Number): Option[Number] = {
      def helper(num: Number, depth: Int = 0): Option[(Option[Int], Number, Option[Int])] = num match {
        case Literal(_)                                        => None
        case Pair(Literal(left), Literal(right)) if depth >= 4 => Some((Some(left), Literal(0), Some(right)))
        case Pair(left, right) =>
          helper(left, depth + 1)
            .map {
              case (leftAdd, left, rightAdd) =>
                (leftAdd, Pair(left, rightAdd.map(right.addLeft).getOrElse(right)), None)
            }
            .orElse {
              helper(right, depth + 1).map {
                case (leftAdd, right, rightAdd) =>
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
    def +(that: Number): Number = Pair(this, that).reduce

    def addLeft(addValue: Int): Number
    def addRight(addValue: Int): Number
    def magnitude: Int
  }
  private case class Literal(value: Int) extends Number {
    def addLeft(addValue: Int): Number  = Literal(value + addValue)
    def addRight(addValue: Int): Number = Literal(value + addValue)
    def magnitude: Int                  = value
  }
  private case class Pair(left: Number, right: Number) extends Number {
    def addLeft(addValue: Int): Number  = this.copy(left = left.addLeft(addValue))
    def addRight(addValue: Int): Number = this.copy(right = right.addRight(addValue))
    def magnitude: Int                  = (3 * left.magnitude) + (2 * right.magnitude)
  }

  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day18.txt"))(_.getLines().toSeq.map(parseNumber))
    val part1 = input.reduceLeft(_ + _).magnitude
    println(s"Part 1: $part1")
    val part2 = input.combinations(2).map(_.reduceLeft(_ + _).magnitude).max
    println(s"Part 2: $part2")
  }

  private def parseNumber(string: String): Number = {
    def number: Parser[Number] = {
      wholeNumber ^^ (_.toInt) ^^ Literal.apply |
        "[" ~> number ~ "," ~ number <~ "]" ^^ { case left ~ _ ~ right => Pair(left, right) }
    }

    parseAll(number, string).get
  }
}
