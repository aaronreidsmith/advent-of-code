package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.io.Source
import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox
import scala.util.parsing.combinator.JavaTokenParsers

object Day07 extends Solution with JavaTokenParsers {
  type I  = String
  type O1 = String
  type O2 = String

  private def quotedIdent: Parser[String] = ident ^^ (i => s"`$i`")
  private def operation: Parser[String] = {
    def expression = quotedIdent | wholeNumber
    def and        = (expression <~ "AND") ~ expression ^^ { case left ~ right => s"$left & $right" }
    def or         = (expression <~ "OR") ~ expression ^^ { case left ~ right => s"$left | $right" }
    def lshift     = (expression <~ "LSHIFT") ~ expression ^^ { case wire ~ bits => s"$wire << $bits" }
    def rshift     = (expression <~ "RSHIFT") ~ expression ^^ { case wire ~ bits => s"$wire >> $bits" }
    def not        = "NOT" ~> expression ^^ (i => s"~$i")
    and | or | lshift | rshift | not | expression
  }

  private def line: Parser[String] = (operation <~ "->") ~ quotedIdent ^^ {
    case source ~ target => s"lazy val $target = $source"
  }

  private val toolBox = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

  private def wires(input: String): String = parse(rep(line), input).get.mkString("; ")

  override def parseInput(file: Source): String = file.getLines().mkString("\n")

  override def part1(input: String): String = {
    val tree = s"object Part1 { ${wires(input)} }; Part1.a"
    toolBox.eval(toolBox.parse(tree)).toString
  }

  override def part2(input: String): String = {
    val tree =
      s"""trait Base { ${wires(input)} }
         |object Part1 extends Base
         |object Part2 extends Base { override lazy val b = Part1.a }
         |Part2.a""".stripMargin
    toolBox.eval(toolBox.parse(tree)).toString
  }
}
