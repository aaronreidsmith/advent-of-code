package io.github.aaronreidsmith.year2016

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day23 {
  private val cpy = "^cpy (.*) (.*)$".r
  private val inc = "^inc (.*)$".r
  private val dec = "^dec (.*)$".r
  private val jnz = "^jnz (.*) (.*)$".r
  private val tgl = "^tgl (.*)$".r

  private implicit class RichString(str: String) {
    def toIntOrValue(registers: Map[String, Int]): Int =
      Try(str.toInt).getOrElse(registers.getOrElse(str, throw new IllegalArgumentException(str)))
  }

  def main(args: Array[String]): Unit = {
    val input        = Source.fromResource("2016/day23.txt")
    val instructions = input.getLines().toVector
    input.close()

    println(s"Part 1: ${part1(instructions)}")
    // Adapted from https://www.reddit.com/r/adventofcode/comments/5jvbzt/2016_day_23_solutions/dbjbqtq
    println(s"Part 2: ${77 * 73 + factorial(12)}")
  }

  @tailrec
  private def part1(
      instructions: Vector[String],
      registers: Map[String, Int] = Map("a" -> 7, "b" -> 0, "c" -> 0, "d" -> 0),
      pointer: Int = 0
  ): Long = if (pointer < 0 || pointer >= instructions.length) {
    registers("a")
  } else {
    val (updatedRegisters, updatedPointer, updatedInstructions) = instructions(pointer) match {
      case cpy(maybeValue, register) =>
        // If this works, this is an invalid instruction and should be skipped
        if (Try(register.toInt).isSuccess) (registers, pointer + 1, instructions)
        else (registers.updated(register, maybeValue.toIntOrValue(registers)), pointer + 1, instructions)
      case inc(register) =>
        (
          registers.updated(register, registers(register) + 1),
          pointer + 1,
          instructions
        )
      case dec(register) => (registers.updated(register, registers(register) - 1), pointer + 1, instructions)
      case jnz(maybeValue, jumpSize) =>
        (
          registers,
          if (maybeValue.toIntOrValue(registers) != 0) pointer + jumpSize.toIntOrValue(registers) else pointer + 1,
          instructions
        )
      case tgl(maybeValue) =>
        val index = pointer + maybeValue.toIntOrValue(registers)
        val newInstructions = if (index < 0 || index >= instructions.length) {
          instructions
        } else {
          instructions.updated(index, toggle(instructions(index)))
        }
        (registers, pointer + 1, newInstructions)
      case _ => throw new IllegalArgumentException
    }
    part1(updatedInstructions, updatedRegisters, updatedPointer)
  }

  @tailrec
  private def factorial(n: Int, acc: Int = 1): Int = if (n == 1) acc else factorial(n - 1, acc * n)

  private def toggle(instruction: String): String = instruction match {
    case inc(_)    => instruction.replace("inc", "dec")
    case dec(_)    => instruction.replace("dec", "inc")
    case tgl(_)    => instruction.replace("tgl", "inc")
    case jnz(_, _) => instruction.replace("jnz", "cpy")
    case cpy(_, _) => instruction.replace("cpy", "jnz")
    case other     => throw new IllegalArgumentException(other)
  }
}
