package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day04 extends Solution {
  type I  = Map[Int, Card]
  type O1 = Int
  type O2 = Int

  case class Card(number: Int, winningNumbers: Set[Int], myNumbers: Set[Int]) {
    def numMatches: Int = winningNumbers.intersect(myNumbers).size
    def score: Int = {
      if (numMatches == 0) {
        0
      } else {
        (0 until numMatches - 1).foldLeft(1)((acc, _) => acc * 2)
      }
    }
  }

  override def parseInput(file: Source): Map[Int, Card] = file
    .getLines()
    .toList
    .map { line =>
      val Array(card, numbers, _*)                   = line.split(':'): @unchecked
      val cardNumber                                 = card.filter(_.isDigit).toInt
      val Array(winningNumbersRaw, myNumbersRaw, _*) = numbers.split('|'): @unchecked
      val winningNumbers                             = winningNumbersRaw.split("\\s+").flatMap(_.trim.toIntOption).toSet
      val myNumbers                                  = myNumbersRaw.split("\\s+").flatMap(_.trim.toIntOption).toSet
      cardNumber -> Card(cardNumber, winningNumbers, myNumbers)
    }
    .toMap

  override def part1(input: Map[Int, Card]): Int = input.values.foldLeft(0)(_ + _.score)

  override def part2(input: Map[Int, Card]): Int = {
    @tailrec
    def helper(remaining: List[Card], total: Int = 0): Int = remaining match {
      case Nil => total
      case head :: tail =>
        val newCards = if (head.numMatches == 0) {
          Nil
        } else {
          (head.number + 1 to head.number + head.numMatches).map(input(_)).toList
        }
        helper(newCards ::: tail, total + 1)
    }

    helper(input.values.toList)
  }
}
