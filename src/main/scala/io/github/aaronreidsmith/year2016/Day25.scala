package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

object Day25 {
  private val cpy = "^cpy (.*) (.*)$".r
  private val inc = "^inc (.*)$".r
  private val dec = "^dec (.*)$".r
  private val jnz = "^jnz (.*) (.*)$".r
  private val out = "^out (.*)$".r

  private val endState = "01" * 50

  private implicit class RichString(str: String) {
    def toIntOrValue(registers: Map[String, Int]): Int = Try(str.toInt) match {
      case Success(int) => int
      case Failure(_) =>
        registers.get(str) match {
          case Some(int) => int
          case None      => throw new IllegalArgumentException(str)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val instructions = Using.resource(Source.fromResource("2016/day25.txt"))(_.getLines().toVector)
    val part1        = LazyList.from(0).find(i => solution(instructions, Map("a" -> i, "b" -> 0, "c" -> 0, "d" -> 0))).get
    println(s"Part 1: $part1")
  }

  @tailrec
  private def solution(
      instructions: Vector[String],
      registers: Map[String, Int],
      pointer: Int = 0,
      output: StringBuilder = new StringBuilder
  ): Boolean = if (output.length >= 100) {
    output.take(100).mkString == endState
  } else {
    val (updatedRegisters, updatedPointer, updatedOutput) = instructions(pointer) match {
      case cpy(maybeValue, register) =>
        (registers.updated(register, maybeValue.toIntOrValue(registers)), pointer + 1, output)
      case inc(register) => (registers.updated(register, registers(register) + 1), pointer + 1, output)
      case dec(register) => (registers.updated(register, registers(register) - 1), pointer + 1, output)
      case jnz(maybeValue, jumpSize) =>
        (registers, if (maybeValue.toIntOrValue(registers) != 0) pointer + jumpSize.toInt else pointer + 1, output)
      case out(maybeValue) =>
        (registers, pointer + 1, output.append(maybeValue.toIntOrValue(registers)))
      case _ => throw new IllegalArgumentException
    }
    solution(instructions, updatedRegisters, updatedPointer, updatedOutput)
  }
}
