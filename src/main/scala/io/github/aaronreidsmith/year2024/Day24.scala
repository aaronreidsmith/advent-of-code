package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.io.Source
import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox
import scala.util.parsing.combinator.JavaTokenParsers

object Day24 extends Solution with JavaTokenParsers {
  type I  = String
  type O1 = String
  type O2 = String

  override def parseInput(file: Source): String = file.mkString

  // Almost directly taken from my 2015, day 7 solution
  override def part1(input: String): String = {
    def quotedIdent: Parser[String] = ident ^^ (i => s"`$i`")

    def operation: Parser[String] = {
      def and = (quotedIdent <~ "AND") ~ quotedIdent ^^ { case left ~ right => s"$left & $right" }
      def or  = (quotedIdent <~ "OR") ~ quotedIdent ^^ { case left ~ right => s"$left | $right" }
      def xor = (quotedIdent <~ "XOR") ~ quotedIdent ^^ { case left ~ right => s"$left ^ $right" }

      and | or | xor
    }

    def line: Parser[String] = {
      def known   = (quotedIdent <~ ":") ~ wholeNumber ^^ { case left ~ right => s"val $left = $right" }
      def unknown = (operation <~ "->") ~ quotedIdent ^^ { case source ~ target => s"lazy val $target = $source" }
      known | unknown
    }

    def wires(input: String): String = parse(rep(line), input).get.mkString("; ")

    val outputs = input
      .split("\n")
      .collect { case line if line.contains('z') => s"`${line.split(" -> ").last}`" }
      .toList
      .sorted
      .reverse
    val tree =
      s"""object Part1 {
         |  ${wires(input)}
         |  val answer = BigInt(Seq(${outputs.mkString(", ")}).mkString, 2)
         |}
         |
         |Part1.answer""".stripMargin

    val toolBox = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()
    universe.runtimeMirror(getClass.getClassLoader).mkToolBox().eval(toolBox.parse(tree)).toString
  }

  override def part2(input: String): String = {
    "Done by hand"
  }
}
