package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

@Slow(part2 = true)
object Day12 extends Solution {
  type I  = Vector[String]
  type O1 = Int
  type O2 = Int

  extension (string: String) {
    def toIntOrValue(registers: Map[String, Int]): Int = Try(string.toInt) match {
      case Success(int) => int
      case Failure(_) =>
        registers.get(string) match {
          case Some(int) => int
          case None      => throw new IllegalArgumentException(string)
        }
    }
  }

  override def parseInput(file: Source): Vector[String] = file.getLines().toVector
  override def part1(input: Vector[String]): Int = {
    solution(input, Map("a" -> 0, "b" -> 0, "c" -> 0, "d" -> 0))
  }
  override def part2(input: Vector[String]): Int = {
    solution(input, Map("a" -> 0, "b" -> 0, "c" -> 1, "d" -> 0))
  }

  // Top-level so they are only compiled once
  private val cpy = "^cpy (.*) (.*)$".r
  private val inc = "^inc (.*)$".r
  private val dec = "^dec (.*)$".r
  private val jnz = "^jnz (.*) (.*)$".r
  @tailrec
  private def solution(instructions: Vector[String], registers: Map[String, Int], pointer: Int = 0): Int = {
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
}
