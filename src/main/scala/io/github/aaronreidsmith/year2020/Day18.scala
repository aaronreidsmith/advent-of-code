package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

object Day18 extends Solution {
  type I  = Parser => Long
  type O1 = Long
  type O2 = Long

  sealed trait Parser extends JavaTokenParsers {
    def expr: this.Parser[Long]

    // Concrete methods
    def factor: this.Parser[Long] = number | optionalWhitespace("(") ~> expr <~ optionalWhitespace(")")
    def number: this.Parser[Long] = wholeNumber ^^ (_.toLong)
    def optionalWhitespace[T](p: => this.Parser[T]): this.Parser[T] = {
      val optionalWhiteSpaceRegex = """\s*""".r
      optionalWhiteSpaceRegex ~> p <~ optionalWhiteSpaceRegex
    }
  }

  override def parseInput(file: Source): Parser => Long = {
    val expressions = file.getLines().toList
    (parser: Parser) => expressions.foldLeft(0L)((acc, line) => acc + parser.parseAll(parser.expr, line).getOrElse(0L))
  }

  override def part1(input: Parser => Long): Long = input(new Parser {
    override def expr: this.Parser[Long] = {
      factor ~ rep((optionalWhitespace("+") ~ factor) | (optionalWhitespace("*") ~ factor)) ^^ {
        case num ~ list =>
          list.foldLeft(num) {
            case (acc, "+" ~ y) => acc + y
            case (acc, "*" ~ y) => acc * y
            case (acc, _)       => acc
          }
      }
    }
  })

  override def part2(input: Parser => Long): Long = input(new Parser {
    override def expr: this.Parser[Long] = term ~ rep(optionalWhitespace("*") ~> term) ^^ {
      case x ~ y => y.foldLeft(x)(_ * _)
    }

    private def term: this.Parser[Long] = factor ~ rep(optionalWhitespace("+") ~> factor) ^^ {
      case x ~ y => y.foldLeft(x)(_ + _)
    }
  })
}
