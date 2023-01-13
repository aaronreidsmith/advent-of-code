package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution
import org.apache.commons.math3.primes.Primes.isPrime

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day23 extends Solution {
  type I  = Vector[String]
  type O1 = Int
  type O2 = Int

  // Top level so we only have to compile once
  private val set = "^set (.*) (.*)$".r
  private val sub = "^sub (.*) (.*)$".r
  private val mul = "^mul (.*) (.*)$".r
  private val jnz = "^jnz (.*) (.*)$".r

  override def parseInput(file: Source): Vector[String] = file.getLines().toVector

  override def part1(input: Vector[String]): Int = {
    @tailrec
    def helper(
        position: Int,
        registers: Map[String, Int] = ('a' to 'h').map(_.toString -> 0).toMap,
        mulsSeen: Int = 0
    ): Int = if (position < 0 || position >= input.size) {
      mulsSeen
    } else {
      input(position) match {
        case set(register, value) =>
          val newValue         = getValue(registers, value)
          val updatedRegisters = registers + (register -> newValue)
          helper(position + 1, updatedRegisters, mulsSeen)
        case sub(register, value) =>
          val existingValue    = registers(register)
          val delta            = getValue(registers, value)
          val updatedRegisters = registers + (register -> (existingValue - delta))
          helper(position + 1, updatedRegisters, mulsSeen)
        case mul(register, value) =>
          val existingValue    = registers(register)
          val factor           = getValue(registers, value)
          val updatedRegisters = registers + (register -> (existingValue * factor))
          helper(position + 1, updatedRegisters, mulsSeen + 1)
        case jnz(value, offset) =>
          val jump  = getValue(registers, offset)
          val check = getValue(registers, value)
          val delta = if (check != 0) jump else 1
          helper(position + delta, registers, mulsSeen)
        case _ => throw new IllegalArgumentException
      }
    }
    helper(0)
  }

  // The program basically sets registers b and c to 2 values 17,000 numbers apart and counts the non-primes between
  // them using the other registers as space to do its brute-force calculation. Since we were able to deduce that, we
  // can just find the initial values of b and c and do it ourselves
  override def part2(input: Vector[String]): Int = {
    val bc = Set("b", "c")
    val initial = input.takeWhile(_ != "sub b -17").foldLeft(Map("b" -> 0, "c" -> 0)) {
      case (registers, set(register, value)) if bc.contains(register) =>
        val newValue = getValue(registers, value)
        registers + (register -> newValue)
      case (registers, sub(register, value)) if bc.contains(register) =>
        val existingValue = registers(register)
        val delta         = getValue(registers, value)
        registers + (register -> (existingValue - delta))
      case (registers, mul(register, value)) if bc.contains(register) =>
        val existingValue = registers(register)
        val factor        = getValue(registers, value)
        registers + (register -> (existingValue * factor))
      case (registers, _) => registers
    }

    (initial("b") to initial("c") by 17).count(n => !isPrime(n))
  }

  private def getValue(registers: Map[String, Int], maybeValue: String): Int = Try(maybeValue.toInt) match {
    case Success(value) => value
    case Failure(_)     => registers.getOrElse(maybeValue, 0)
  }
}
