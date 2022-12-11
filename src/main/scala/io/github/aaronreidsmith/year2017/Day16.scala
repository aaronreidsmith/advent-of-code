package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day16 extends Solution(2017, 16) {
  type I  = List[Move]
  type O1 = String
  type O2 = String

  private[year2017] sealed trait Move {
    def perform(state: String): String
  }
  private case class Spin(x: Int) extends Move {
    def perform(state: String): String = state.takeRight(x) ++: state.dropRight(x)
  }
  private case class Exchange(a: Int, b: Int) extends Move {
    def perform(state: String): String = state.updated(a, state(b)).updated(b, state(a))
  }
  private case class Partner(a: Char, b: Char) extends Move {
    def perform(state: String): String = Exchange(state.indexOf(a), state.indexOf(b)).perform(state)
  }

  override protected[year2017] def parseInput(file: Source): List[Move] = {
    val spin     = "^s(\\d+)$".r
    val exchange = "^x(\\d+)/(\\d+)$".r
    val partner  = "^p(.*)/(.*)$".r
    file.mkString.trim.split(',').toList.collect {
      case spin(amount)                   => Spin(amount.toInt)
      case exchange(position1, position2) => Exchange(position1.toInt, position2.toInt)
      case partner(char1, char2)          => Partner(char1.charAt(0), char2.charAt(0))
    }
  }

  override protected[year2017] def part1(input: List[Move]): String = dance(input, ('a' to 'p').mkString)

  override protected[year2017] def part2(input: List[Move]): String = {
    val initialState    = ('a' to 'p').mkString
    val infiniteDancing = LazyList.iterate(initialState)(dance(input, _))
    val period          = infiniteDancing.indexOf(initialState, 1)
    infiniteDancing.drop(1000000000 % period).head
  }

  private def dance(moves: List[Move], state: String): String = moves.foldLeft(state)((acc, move) => move.perform(acc))
}
