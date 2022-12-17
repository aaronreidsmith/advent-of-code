package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.annotations.Slow

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

@Slow(part1 = true)
object Day25 extends Solution(2016, 25) {
  type I  = Vector[String]
  type O1 = Int
  type O2 = Nothing

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

  override protected[year2016] def parseInput(file: Source): Vector[String] = file.getLines().toVector
  override protected[year2016] def part1(input: Vector[String]): Int = {
    Iterator.from(0).find(i => solution(input, Map("a" -> i, "b" -> 0, "c" -> 0, "d" -> 0))).get
  }

  // Top-level so these are only defined once
  private val cpy      = "^cpy (.*) (.*)$".r
  private val inc      = "^inc (.*)$".r
  private val dec      = "^dec (.*)$".r
  private val jnz      = "^jnz (.*) (.*)$".r
  private val out      = "^out (.*)$".r
  private val endState = "01" * 50
  private def solution(instructions: Vector[String], registers: Map[String, Int]): Boolean = {
    val output = new StringBuilder
    @tailrec
    def helper(registers: Map[String, Int], pointer: Int = 0): Boolean = if (output.length >= 100) {
      output.take(100).mkString == endState
    } else {
      val (updatedRegisters, updatedPointer) = instructions(pointer) match {
        case cpy(maybeValue, register) => (registers.updated(register, maybeValue.toIntOrValue(registers)), pointer + 1)
        case inc(register)             => (registers.updated(register, registers(register) + 1), pointer + 1)
        case dec(register)             => (registers.updated(register, registers(register) - 1), pointer + 1)
        case jnz(maybeValue, jumpSize) =>
          (registers, if (maybeValue.toIntOrValue(registers) != 0) pointer + jumpSize.toInt else pointer + 1)
        case out(maybeValue) =>
          output.append(maybeValue.toIntOrValue(registers))
          (registers, pointer + 1)
        case _ => throw new IllegalArgumentException
      }
      helper(updatedRegisters, updatedPointer)
    }

    helper(registers)
  }
}
