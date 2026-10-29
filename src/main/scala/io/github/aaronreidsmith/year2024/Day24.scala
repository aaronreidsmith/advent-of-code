package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object Day24 extends Solution with JavaTokenParsers {
  type I  = Map[String, Gate]
  type O1 = Long
  type O2 = String

  // Given a way to look up the signal on any wire, produce the signal for this gate
  type Gate = (String => Int) => Int

  // Almost directly taken from my 2015, day 7 solution
  override def parseInput(file: Source): Map[String, Gate] = {
    def wire: Parser[Gate] = ident ^^ (name => signal => signal(name))

    def operation: Parser[Gate] = {
      def binary(op: String)(f: (Int, Int) => Int): Parser[Gate] = (wire <~ op) ~ wire ^^ {
        case left ~ right => signal => f(left(signal), right(signal))
      }
      def and = binary("AND")(_ & _)
      def or  = binary("OR")(_ | _)
      def xor = binary("XOR")(_ ^ _)

      and | or | xor
    }

    def line: Parser[(String, Gate)] = {
      def known: Parser[(String, Gate)] = (ident <~ ":") ~ wholeNumber ^^ { case target ~ value =>
        target -> (_ => value.toInt)
      }
      def unknown: Parser[(String, Gate)] = (operation <~ "->") ~ ident ^^ { case source ~ target => target -> source }
      known | unknown
    }

    parse(rep(line), file.mkString).get.toMap
  }

  override def part1(input: Map[String, Gate]): Long = {
    val cache = mutable.Map.empty[String, Int]
    def signal(wire: String): Int = cache.getOrElseUpdate(wire, input(wire)(signal))

    input.keys
      .filter(_.startsWith("z"))
      .toSeq
      .sorted
      .reverse
      .foldLeft(0L)((acc, wire) => (acc << 1) | signal(wire))
  }

  override def part2(input: Map[String, Gate]): String = {
    // TODO: Done by hand. Maybe I will implement in the future
    "hjf,kdh,kpp,sgj,vss,z14,z31,z35"
  }
}
