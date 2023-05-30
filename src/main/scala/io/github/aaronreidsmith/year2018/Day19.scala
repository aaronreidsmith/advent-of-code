package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution
import org.apache.commons.math3.primes.Primes

import scala.annotation.tailrec
import scala.io.Source
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}

object Day19 extends Solution {
  type I  = Vector[String]
  type O1 = Int
  type O2 = Long

  override def parseInput(file: Source): Vector[String] = file.getLines().toVector

  override def part1(input: Vector[String]): Int = {
    val instructionPointer = input.head.last.asDigit
    val instructions       = input.tail

    @tailrec
    def helper(registers: Map[Int, Int]): Int = registers.get(instructionPointer) match {
      case Some(ipValue) =>
        // When the instruction pointer is bound to a register, its value is written to that register just before each instruction is executed
        val withInstructionPointerUpdated = registers.updated(instructionPointer, ipValue)
        Try(instructions(ipValue)) match {
          case Success(instruction) =>
            val Array(instructionType, aStr, bStr, cStr, _*) = instruction.split(' '): @unchecked
            val a                                            = aStr.toInt
            val b                                            = bStr.toInt
            val c                                            = cStr.toInt
            val withInstructionApplied = instructionType match {
              case "addr" => withInstructionPointerUpdated.updated(c, registers(a) + registers(b))
              case "addi" => withInstructionPointerUpdated.updated(c, registers(a) + b)
              case "mulr" => withInstructionPointerUpdated.updated(c, registers(a) * registers(b))
              case "muli" => withInstructionPointerUpdated.updated(c, registers(a) * b)
              case "banr" => withInstructionPointerUpdated.updated(c, registers(a) & registers(b))
              case "bani" => withInstructionPointerUpdated.updated(c, registers(a) & b)
              case "borr" => withInstructionPointerUpdated.updated(c, registers(a) | registers(b))
              case "bori" => withInstructionPointerUpdated.updated(c, registers(a) | b)
              case "setr" => withInstructionPointerUpdated.updated(c, registers(a))
              case "seti" => withInstructionPointerUpdated.updated(c, a)
              case "gtir" => withInstructionPointerUpdated.updated(c, if (a > registers(b)) 1 else 0)
              case "gtri" => withInstructionPointerUpdated.updated(c, if (registers(a) > b) 1 else 0)
              case "gtrr" => withInstructionPointerUpdated.updated(c, if (registers(a) > registers(b)) 1 else 0)
              case "eqir" => withInstructionPointerUpdated.updated(c, if (a == registers(b)) 1 else 0)
              case "eqri" => withInstructionPointerUpdated.updated(c, if (registers(a) == b) 1 else 0)
              case "eqrr" => withInstructionPointerUpdated.updated(c, if (registers(a) == registers(b)) 1 else 0)
              case _      => throw new IllegalArgumentException
            }
            // and the value of that register is written back to the instruction pointer immediately after each instruction finishes execution
            helper(withInstructionApplied.updated(instructionPointer, withInstructionApplied(instructionPointer) + 1))
          case Failure(_) => registers(0)
        }
      case None => registers(0)
    }

    helper((0 until 6).map(_ -> 0).toMap)
  }

  // Adapted from https://www.reddit.com/r/adventofcode/comments/a7j9zc/comment/ec3w9wy
  override def part2(input: Vector[String]): Long = {
    val Seq(a, b, _*)     = Seq(22, 24).map(index => input(index).split(' ')(2).toInt): @unchecked
    val numberToFactorize = 10551236 + a * 22 + b
    Primes.primeFactors(numberToFactorize).asScala.foldLeft(1L) { (acc, primeFactor) =>
      acc * ((math.pow(primeFactor.toDouble, 2).toLong - 1) / (primeFactor - 1))
    }
  }
}
