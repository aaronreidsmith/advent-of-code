package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day22 extends Solution {
  type I  = (Vector[Int], Vector[Int])
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): (Vector[Int], Vector[Int]) = {
    val Array(p1, p2, _*) = file.mkString.trim
      .split("\n\n")
      .map(player => player.split('\n').toVector.tail.map(_.toInt))
    (p1, p2)
  }

  override def part1(input: (Vector[Int], Vector[Int])): Int = {
    @tailrec
    def helper(player1: Vector[Int], player2: Vector[Int]): Int = if (player1.isEmpty) {
      calculateScore(player2)
    } else if (player2.isEmpty) {
      calculateScore(player1)
    } else {
      val player1Card = player1.head
      val player2Card = player2.head
      if (player1Card > player2Card) {
        helper(player1.tail :+ player1Card :+ player2Card, player2.tail)
      } else {
        helper(player1.tail, player2.tail :+ player2Card :+ player1Card)
      }
    }

    val (p1, p2) = input
    helper(p1, p2)
  }

  override def part2(input: (Vector[Int], Vector[Int])): Int = {
    // TODO: This is copied from Python and not idiomatic
    def helper(player1: mutable.Buffer[Int], player2: mutable.Buffer[Int]): (Seq[Int], Seq[Int]) = {
      val seen = mutable.Set.empty[(Vector[Int], Vector[Int])]

      while (player1.nonEmpty && player2.nonEmpty) {
        val key = (player1.toVector, player2.toVector)
        if (seen.contains(key)) {
          return (player1.toSeq, Seq())
        } else {
          seen.add(key)
        }

        val p1Card = player1.remove(0)
        val p2Card = player2.remove(0)
        if (p1Card <= player1.length && p2Card <= player2.length) {
          val p1Prime                  = player1.take(p1Card)
          val p2Prime                  = player2.take(p2Card)
          val (newP1Prime, newP2Prime) = helper(p1Prime, p2Prime)
          if (newP1Prime.length > newP2Prime.length) {
            player1.appendAll(Seq(p1Card, p2Card))
          } else {
            player2.appendAll(Seq(p2Card, p1Card))
          }
        } else if (p1Card > p2Card) {
          player1.appendAll(Seq(p1Card, p2Card))
        } else {
          player2.appendAll(Seq(p2Card, p1Card))
        }
      }

      (player1.toSeq, player2.toSeq)
    }

    val (player1, player2) = input
    val (p1, p2)           = helper(player1.toBuffer, player2.toBuffer)
    calculateScore(p1 ++ p2)
  }

  private def calculateScore(deck: Seq[Int]): Int = deck.reverse.zipWithIndex.foldLeft(0) {
    case (acc, (value, index)) => acc + ((index + 1) * value)
    case (acc, _)              => acc
  }
}
