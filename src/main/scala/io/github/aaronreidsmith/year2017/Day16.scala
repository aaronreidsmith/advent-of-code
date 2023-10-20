package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day16 extends Solution {
  type I  = List[Move]
  type O1 = String
  type O2 = String

  enum Move {
    case Spin(x: Int)
    case Exchange(a: Int, b: Int)
    case Partner(a: Char, b: Char)

    def perform(state: String): String = this match {
      case Spin(x)        => state.takeRight(x) ++: state.dropRight(x)
      case Exchange(a, b) => state.updated(a, state(b)).updated(b, state(a))
      case Partner(a, b)  => Exchange(state.indexOf(a), state.indexOf(b)).perform(state)
    }
  }

  override def parseInput(file: Source): List[Move] = {
    val spin     = """^s(\d+)$""".r
    val exchange = """^x(\d+)/(\d+)$""".r
    val partner  = """^p(.*)/(.*)$""".r
    file.mkString.trim.split(',').toList.collect {
      case spin(amount)                   => Move.Spin(amount.toInt)
      case exchange(position1, position2) => Move.Exchange(position1.toInt, position2.toInt)
      case partner(char1, char2)          => Move.Partner(char1.charAt(0), char2.charAt(0))
    }
  }

  override def part1(input: List[Move]): String = dance(input, ('a' to 'p').mkString)

  override def part2(input: List[Move]): String = {
    val initialState    = ('a' to 'p').mkString
    val infiniteDancing = LazyList.iterate(initialState)(dance(input, _))
    val period          = infiniteDancing.indexOf(initialState, 1)
    infiniteDancing.drop(1000000000 % period).head
  }

  private def dance(moves: List[Move], state: String): String = moves.foldLeft(state)((acc, move) => move.perform(acc))
}
