package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day22 extends Solution(2019, 22) {
  type I  = List[String]
  type O1 = Int
  type O2 = BigInt

  // Top-level so we only have to compile once
  private val dealIncrement = """^deal with increment (\d+)$""".r
  private val cut           = """^cut (-?\d+)$""".r

  private[year2019] case class Deck(cards: Vector[Int]) {
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

  override protected[year2019] def parseInput(file: Source): List[String] = file.getLines().toList

  override protected[year2019] def part1(input: List[String]): Int = {
    input
      .foldLeft(Deck((0 until 10_007).toVector)) {
        case (acc, dealIncrement(n))      => acc.deal(n.toInt)
        case (acc, cut(n))                => acc.cut(n.toInt)
        case (acc, "deal into new stack") => acc.deal
        case (acc, _)                     => acc
      }
      .cards
      .indexOf(2019)
  }

  // Adapted from https://todd.ginsberg.com/post/advent-of-code/2019/day22/
  override protected[year2019] def part2(input: List[String]): BigInt = {
    val two      = BigInt(2)
    val numCards = BigInt(119315717514047L)
    val shuffles = BigInt(101741582076661L)
    val target   = BigInt(2020)

    val memory = ArrayBuffer(BigInt(1), BigInt(0))
    input.reverse.foreach { instruction =>
      instruction match {
        case dealIncrement(n) =>
          val multiplier = BigInt(n).modPow(numCards - two, numCards)
          memory(0) *= multiplier
          memory(1) *= multiplier
        case cut(n) => memory(1) += BigInt(n)
        case "deal into new stack" =>
          memory(0) = -memory(0)
          memory(1) = -(memory(1) + 1)
        case other => throw new IllegalArgumentException(s"'$other' is not a valid instruction'")
      }
      memory(0) %= numCards
      memory(1) %= numCards
    }
    val power = memory(0).modPow(shuffles, numCards)
    (
      (power * target) + (
        (memory(1) * (power + numCards - 1)) *
          (memory(0) - 1).modPow(numCards - two, numCards)
      )
    ).mod(numCards)
  }
}
