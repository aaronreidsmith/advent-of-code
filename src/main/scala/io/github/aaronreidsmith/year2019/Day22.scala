package io.github.aaronreidsmith.year2019

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using

object Day22 {
  private val dealIncrement = "^deal with increment (\\d+)$".r
  private val cut           = "^cut (-?\\d+)$".r

  private case class Deck(cards: Vector[Int]) {
    def cut(n: Int): Deck = {
      val newCards = if (n >= 0) cards.drop(n) ++ cards.take(n) else cards.takeRight(n.abs) ++ cards.dropRight(n.abs)
      Deck(newCards)
    }
    def deal: Deck = Deck(cards.reverse)
    def deal(n: Int): Deck = {
      val deckSize = cards.size
      val buffer   = ArrayBuffer.fill(deckSize)(0)

      @tailrec
      def helper(index: Int = 0, pointer: Int = 0): Deck = if (index >= deckSize) {
        Deck(buffer.toVector)
      } else {
        buffer(pointer) = cards(index)
        helper(index + 1, (pointer + n) % deckSize)
      }

      helper()
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2019/day22.txt"))(_.getLines().toList)
    val part1 = input
      .foldLeft(Deck((0 until 10007).toVector)) { (acc, line) =>
        line match {
          case dealIncrement(n)      => acc.deal(n.toInt)
          case cut(n)                => acc.cut(n.toInt)
          case "deal into new stack" => acc.deal
          case other                 => throw new IllegalArgumentException(s"'$other' is not a valid instruction'")
        }
      }
      .cards
      .indexOf(2019)
    println(s"Part 1: $part1")

    // I am not smart enough to figure this out
    // Adapted from https://todd.ginsberg.com/post/advent-of-code/2019/day22/
    val part2 = {
      val TWO             = BigInt(2)
      val NUMBER_OF_CARDS = BigInt(119315717514047L)
      val SHUFFLES        = BigInt(101741582076661L)
      val FIND            = BigInt(2020)

      val memory = ArrayBuffer(BigInt(1), BigInt(0))
      input.reverse.foreach { instruction =>
        instruction match {
          case dealIncrement(n) =>
            val multiplier = BigInt(n).modPow(NUMBER_OF_CARDS - TWO, NUMBER_OF_CARDS)
            memory(0) *= multiplier
            memory(1) *= multiplier
          case cut(n) => memory(1) += BigInt(n)
          case "deal into new stack" =>
            memory(0) = -memory(0)
            memory(1) = -(memory(1) + 1)
          case other => throw new IllegalArgumentException(s"'$other' is not a valid instruction'")
        }
        memory(0) %= NUMBER_OF_CARDS
        memory(1) %= NUMBER_OF_CARDS
      }
      val power = memory(0).modPow(SHUFFLES, NUMBER_OF_CARDS)
      ((power * FIND) +
        ((memory(1) * (power + NUMBER_OF_CARDS - 1)) *
          (memory(0) - 1).modPow(NUMBER_OF_CARDS - TWO, NUMBER_OF_CARDS)))
        .mod(NUMBER_OF_CARDS)
    }
    println(s"Part 2: $part2")
  }
}
