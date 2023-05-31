package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day23 extends Solution {
  type I  = Vector[String]
  type O1 = Int
  type O2 = Int

  extension (str: String) {
    def toIntOrValue(registers: Map[String, Int]): Int = {
      Try(str.toInt).getOrElse(registers.getOrElse(str, throw new IllegalArgumentException(str)))
    }
  }

  override def parseInput(file: Source): Vector[String] = file.getLines().toVector

  override def part1(input: Vector[String]): Int = {
    val cpy = "^cpy (.*) (.*)$".r
    val inc = "^inc (.*)$".r
    val dec = "^dec (.*)$".r
    val jnz = "^jnz (.*) (.*)$".r
    val tgl = "^tgl (.*)$".r

    def toggle(instruction: String): String = instruction match {
      case inc(_)    => instruction.replace("inc", "dec")
      case dec(_)    => instruction.replace("dec", "inc")
      case tgl(_)    => instruction.replace("tgl", "inc")
      case jnz(_, _) => instruction.replace("jnz", "cpy")
      case cpy(_, _) => instruction.replace("cpy", "jnz")
      case other     => throw new IllegalArgumentException(other)
    }

    @tailrec
    def helper(
        instructions: Vector[String],
        registers: Map[String, Int] = Map("a" -> 7, "b" -> 0, "c" -> 0, "d" -> 0),
        pointer: Int = 0
    ): Int = if (pointer < 0 || pointer >= instructions.length) {
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
      helper(updatedInstructions, updatedRegisters, updatedPointer)
    }

    helper(input)
  }

  // Adapted from https://www.reddit.com/r/adventofcode/comments/5jvbzt/2016_day_23_solutions/dbjbqtq
  override def part2(input: Vector[String]): Int = {
    @tailrec
    def factorial(n: Int, acc: Int = 1): Int = if (n == 1) acc else factorial(n - 1, acc * n)

    // Only want to find the positive 2-digit values for cpy and jnz (one of each)
    val jnz = """^jnz (\d{2}) .*$""".r
    val cpy = """^cpy (\d{2}) .*$""".r

    val a = input.collectFirst { case jnz(value) => value.toInt }.get
    val b = input.collectFirst { case cpy(value) => value.toInt }.get

    a * b + factorial(12)
  }
}
