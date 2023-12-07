package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.implicits.occurrences
import io.github.aaronreidsmith.year2023.Day07.HandType.HighCard

import scala.io.Source
import scala.math.Ordered.orderingToOrdered

object Day07 extends Solution {
  type I  = List[(List[Char], Int)]
  type O1 = Int
  type O2 = Int

  enum Card {
    case Joker, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace
  }

  given Ordering[Card] with {
    override def compare(x: Card, y: Card): Int = x.ordinal.compare(y.ordinal)
  }

  object Card {
    def apply(c: Char, part2: Boolean): Card = c.toUpper match {
      case 'A' => Ace
      case '2' => Two
      case '3' => Three
      case '4' => Four
      case '5' => Five
      case '6' => Six
      case '7' => Seven
      case '8' => Eight
      case '9' => Nine
      case 'T' => Ten
      case 'J' => if (part2) Joker else Jack
      case 'Q' => Queen
      case 'K' => King
      case _   => throw new IllegalArgumentException
    }
  }

  enum HandType {
    case HighCard, Pair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind
  }

  given Ordering[HandType] with {
    override def compare(x: HandType, y: HandType): Int = x.ordinal.compare(y.ordinal)
  }

  case class Hand(cards: List[Card], bet: Int) {
    def handType: HandType = {
      val cardCount = if (cards.contains(Card.Joker)) {
        val highCard = if (cards.forall(_ == Card.Joker)) Card.Joker else cards.occurrences.maxBy(_._2)._1
        cards.map {
          case Card.Joker => highCard
          case other      => other
        }.occurrences
      } else {
        cards.occurrences
      }

      if (cardCount.values.exists(_ == 5)) {
        HandType.FiveOfAKind
      } else if (cardCount.values.exists(_ == 4)) {
        HandType.FourOfAKind
      } else if (cardCount.values.exists(_ == 3)) {
        if (cardCount.values.exists(_ == 2)) {
          HandType.FullHouse
        } else {
          HandType.ThreeOfAKind
        }
      } else if (cardCount.values.exists(_ == 2)) {
        if (cardCount.values.count(_ == 2) == 2) {
          HandType.TwoPair
        } else {
          HandType.Pair
        }
      } else {
        HighCard
      }
    }
  }

  given Ordering[Hand] with {
    override def compare(x: Hand, y: Hand): Int = {
      val naiveComparison = x.handType.compare(y.handType)
      if (naiveComparison != 0) {
        naiveComparison
      } else {
        val (a, b) = x.cards.zip(y.cards).dropWhile((xCard, yCard) => xCard == yCard).head
        a.compare(b)
      }
    }
  }

  override def parseInput(file: Source): List[(List[Char], Int)] = file
    .getLines()
    .map { line =>
      val Array(cards, bet, _*) = line.split("\\s+"): @unchecked
      (cards.toList, bet.toInt)
    }
    .toList

  override def part1(input: List[(List[Char], Int)]): Int = solution(input, false)
  override def part2(input: List[(List[Char], Int)]): Int = solution(input, true)

  private def solution(input: List[(List[Char], Int)], part2: Boolean): Int = input
    .map((cards, bet) => Hand(cards.map(Card(_, part2)), bet))
    .sorted
    .zipWithIndex
    .foldLeft(0) {
      case (acc, (hand, i)) => acc + (i + 1) * hand.bet
    }
}
