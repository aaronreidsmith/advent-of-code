package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day12 {
  private val cpy = "^cpy (.*) (.*)$".r
  private val inc = "^inc (.*)$".r
  private val dec = "^dec (.*)$".r
  private val jnz = "^jnz (.*) (.*)$".r

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
    val input        = Source.fromResource("2016/day12.txt")
    val instructions = input.getLines().toVector
    input.close()

    println(s"Part 1: ${solution(instructions, Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0))}")
    println(s"Part 2: ${solution(instructions, Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0))}")
  }

  @tailrec
  private def solution(instructions: Vector[String], registers: Map[String, Int], pointer: Int = 0): Int =
    if (pointer < 0 || pointer >= instructions.length) {
      registers("a")
    } else {
      val (updatedRegisters, updatedPointer) = instructions(pointer) match {
        case cpy(maybeValue, register) => (registers.updated(register, maybeValue.toIntOrValue(registers)), pointer + 1)
        case inc(register)             => (registers.updated(register, registers(register) + 1), pointer + 1)
        case dec(register)             => (registers.updated(register, registers(register) - 1), pointer + 1)
        case jnz(maybeValue, jumpSize) =>
          (registers, if (maybeValue.toIntOrValue(registers) != 0) pointer + jumpSize.toInt else pointer + 1)
        case _ => throw new IllegalArgumentException
      }
      solution(instructions, updatedRegisters, updatedPointer)
    }
}
