package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.util.parsing.combinator.JavaTokenParsers

object Day18 {
  private sealed trait Parser extends JavaTokenParsers {
    def expr: Parser[Long]

    // Concrete methods
    def factor: Parser[Long] = number | optionalWhitespace("(") ~> expr <~ optionalWhitespace(")")
    def number: Parser[Long] = wholeNumber ^^ (_.toLong)
    def optionalWhitespace[T](p: => Parser[T]): Parser[T] = {
      val optionalWhiteSpaceRegex = """\s*""".r
      optionalWhiteSpaceRegex ~> p <~ optionalWhiteSpaceRegex
    }
  }

  def main(args: Array[String]): Unit = {
    val input = using("2020/day18.txt")(_.getLines().toList)

    val part1Parser = new Parser {
      override def expr: Parser[Long] =
        factor ~ rep((optionalWhitespace("+") ~ factor) | (optionalWhitespace("*") ~ factor)) ^^ {
          case num ~ list =>
            list.foldLeft(num) {
              case (acc, "+" ~ y) => acc + y
              case (acc, "*" ~ y) => acc * y
            }
        }
    }

    val part2Parser = new Parser {
      override def expr: Parser[Long] = term ~ rep(optionalWhitespace("*") ~> term) ^^ {
        case x ~ y => y.foldLeft(x)(_ * _)
      }

      private def term: Parser[Long] = factor ~ rep(optionalWhitespace("+") ~> factor) ^^ {
        case x ~ y => y.foldLeft(x)(_ + _)
      }
    }

    println(s"Part 1: ${solution(input, part1Parser)}")
    println(s"Part 2: ${solution(input, part2Parser)}")
  }

  private def solution(expressions: List[String], parser: Parser): Long = {
    expressions.foldLeft(0L)((acc, line) => acc + parser.parseAll(parser.expr, line).getOrElse(0L))
  }
}
