package io.github.aaronreidsmith.year2018

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

object Day19 {
  private val addr = "^addr (\\d+) (\\d+) (\\d+)$".r
  private val addi = "^addi (\\d+) (\\d+) (\\d+)$".r
  private val mulr = "^mulr (\\d+) (\\d+) (\\d+)$".r
  private val muli = "^muli (\\d+) (\\d+) (\\d+)$".r
  private val banr = "^banr (\\d+) (\\d+) (\\d+)$".r
  private val bani = "^bani (\\d+) (\\d+) (\\d+)$".r
  private val borr = "^borr (\\d+) (\\d+) (\\d+)$".r
  private val bori = "^bori (\\d+) (\\d+) (\\d+)$".r
  private val setr = "^setr (\\d+) (\\d+) (\\d+)$".r
  private val seti = "^seti (\\d+) (\\d+) (\\d+)$".r
  private val gtir = "^gtir (\\d+) (\\d+) (\\d+)$".r
  private val gtri = "^gtri (\\d+) (\\d+) (\\d+)$".r
  private val gtrr = "^gtrr (\\d+) (\\d+) (\\d+)$".r
  private val eqir = "^eqir (\\d+) (\\d+) (\\d+)$".r
  private val eqri = "^eqri (\\d+) (\\d+) (\\d+)$".r
  private val eqrr = "^eqrr (\\d+) (\\d+) (\\d+)$".r

  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2018/day19.txt"))(_.getLines().toVector)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  private def part1(input: Vector[String]): Int = {
    val instructionPointer = input.head.last.asDigit
    val instructions       = input.tail

    // format: off
    @tailrec
    def helper(registers: Map[Int, Int]): Int = registers.get(instructionPointer) match {
      case Some(ipValue) =>
        // When the instruction pointer is bound to a register, its value is written to that register just before each instruction is executed
        val withInstructionPointerUpdated = registers.updated(instructionPointer, ipValue)
        Try(instructions(ipValue)) match {
          case Success(instruction) =>
            val withInstructionApplied = instruction match {
              case addr(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, registers(a.toInt) + registers(b.toInt))
              case addi(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, registers(a.toInt) + b.toInt)
              case mulr(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, registers(a.toInt) * registers(b.toInt))
              case muli(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, registers(a.toInt) * b.toInt)
              case banr(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, registers(a.toInt) & registers(b.toInt))
              case bani(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, registers(a.toInt) & b.toInt)
              case borr(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, registers(a.toInt) | registers(b.toInt))
              case bori(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, registers(a.toInt) | b.toInt)
              case setr(a, _, c) => withInstructionPointerUpdated.updated(c.toInt, registers(a.toInt))
              case seti(a, _, c) => withInstructionPointerUpdated.updated(c.toInt, a.toInt)
              case gtir(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, if (a.toInt > registers(b.toInt)) 1 else 0)
              case gtri(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, if (registers(a.toInt) > b.toInt) 1 else 0)
              case gtrr(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, if (registers(a.toInt) > registers(b.toInt)) 1 else 0)
              case eqir(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, if (a.toInt == registers(b.toInt)) 1 else 0)
              case eqri(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, if (registers(a.toInt) == b.toInt) 1 else 0)
              case eqrr(a, b, c) => withInstructionPointerUpdated.updated(c.toInt, if (registers(a.toInt) == registers(b.toInt)) 1 else 0)
              case other         => throw new IllegalArgumentException(s"'$other' is not a valid pattern")
            }
            // and the value of that register is written back to the instruction pointer immediately after each instruction finishes execution
            val withInstructionPointerReapplied = withInstructionApplied.updated(instructionPointer, withInstructionApplied(instructionPointer) + 1)
            helper(withInstructionPointerReapplied)
          case Failure(_) => registers(0)
        }
      case None => registers(0)
    }
    // format: on

    helper((0 until 6).map(_ -> 0).toMap)
  }

  // Adapted from https://www.reddit.com/r/adventofcode/comments/a7j9zc/comment/ec3w9wy
  private def part2(input: Vector[String]): Long = {
    val Seq(a, b, _*)     = Seq(22, 24).map(index => input(index).split(' ')(2).toInt)
    val numberToFactorize = 10551236 + a * 22 + b
    factorize(numberToFactorize).foldLeft(1L) { (acc, primeFactor) =>
      acc * ((math.pow(primeFactor, 2).toLong - 1) / (primeFactor - 1))
    }
  }

  // Adapted from https://stackoverflow.com/a/30281343
  private def factorize(num: Int): List[Int] = {
    @tailrec
    def help(x: Int, a: Int, acc: List[Int] = Nil): List[Int] = if (a * a < x) {
      if (x % a == 0) help(x / a, a, a :: acc) else help(x, a + 1, acc)
    } else {
      x :: acc
    }

    help(num, 2)
  }
}
