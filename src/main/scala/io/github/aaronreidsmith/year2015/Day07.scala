package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object Day07 extends Solution with JavaTokenParsers {
  type I  = Map[String, Gate]
  type O1 = Int
  type O2 = Int

  // Given a way to look up the signal on any wire, produce the signal for this gate
  type Gate = (String => Int) => Int

  private def operation: Parser[Gate] = {
    def wire: Parser[Gate]       = ident ^^ (name => signal => signal(name))
    def number: Parser[Gate]     = wholeNumber ^^ (value => _ => value.toInt)
    def expression: Parser[Gate] = wire | number
    def binary(op: String)(f: (Int, Int) => Int): Parser[Gate] = (expression <~ op) ~ expression ^^ {
      case left ~ right => signal => f(left(signal), right(signal))
    }
    def and              = binary("AND")(_ & _)
    def or               = binary("OR")(_ | _)
    def lshift           = binary("LSHIFT")((wire, bits) => (wire << bits) & 0xffff)
    def rshift           = binary("RSHIFT")(_ >> _)
    def not: Parser[Gate] = "NOT" ~> expression ^^ (wire => signal => ~wire(signal) & 0xffff)
    and | or | lshift | rshift | not | expression
  }

  private def line: Parser[(String, Gate)] = (operation <~ "->") ~ ident ^^ { case source ~ target => target -> source }

  override def parseInput(file: Source): Map[String, Gate] = parse(rep(line), file.mkString).get.toMap

  override def part1(input: Map[String, Gate]): Int = evaluate(input)("a")

  override def part2(input: Map[String, Gate]): Int = {
    val a = part1(input)
    evaluate(input.updated("b", _ => a))("a")
  }

  private def evaluate(circuit: Map[String, Gate]): String => Int = {
    val cache = mutable.Map.empty[String, Int]
    def signal(wire: String): Int = cache.getOrElseUpdate(wire, circuit(wire)(signal))
    signal
  }
}
