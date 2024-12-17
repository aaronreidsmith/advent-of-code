package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day17 extends Solution {
  type I  = Computer
  type O1 = String
  type O2 = Long

  case class Registers(a: Long, b: Long, c: Long)
  case class Instruction(opcode: Int, operand: Int) {
    def toVector: Vector[Int] = Vector(opcode, operand)
  }
  case class Computer(registers: Registers, instructions: Vector[Instruction]) {
    def run(): Vector[Int] = {
      @tailrec
      def helper(pointer: Int, state: Registers, output: Vector[Int]): Vector[Int] = {
        def comboOperand(operand: Int): Long = {
          operand match {
            case v @ (0 | 1 | 2 | 3) => v.toLong
            case 4                   => state.a
            case 5                   => state.b
            case 6                   => state.c
            case _                   => throw IllegalArgumentException()
          }
        }

        if (pointer < 0 || pointer >= instructions.length) {
          output
        } else {
          val instruction = instructions(pointer)
          instruction.opcode match {
            case 0 =>
              val operand = comboOperand(instruction.operand)
              helper(pointer + 1, state.copy(a = state.a >> operand), output)
            case 1 =>
              helper(pointer + 1, state.copy(b = state.b ^ instruction.operand), output)
            case 2 =>
              val operand = comboOperand(instruction.operand)
              helper(pointer + 1, state.copy(b = operand % 8), output)
            case 3 =>
              if (state.a == 0) {
                helper(pointer + 1, state, output)
              } else {
                helper(instruction.operand, state, output)
              }
            case 4 =>
              helper(pointer + 1, state.copy(b = state.b ^ state.c), output)
            case 5 =>
              val operand = comboOperand(instruction.operand)
              helper(pointer + 1, state, output.appended((operand % 8).toInt))
            case 6 =>
              val operand = comboOperand(instruction.operand)
              helper(pointer + 1, state.copy(b = state.a >> operand), output)
            case 7 =>
              val operand = comboOperand(instruction.operand)
              helper(pointer + 1, state.copy(c = state.a >> operand), output)
            case _ => throw IllegalArgumentException()
          }
        }
      }

      helper(0, registers, Vector.empty)
    }
  }

  override def parseInput(file: Source): Computer = {
    val Array(registersRaw, instructionsRaw, _*) = file.mkString.split("\n\n"): @unchecked
    val registers                                = registersRaw.split('\n').map(_.filter(_.isDigit).toLong)
    val instructions = instructionsRaw
      .split(',')
      .map(_.filter(_.isDigit).toInt)
      .grouped(2)
      .collect { case Array(a, b) => Instruction(a, b) }
      .toVector
    Computer(Registers(registers(0), registers(1), registers(2)), instructions)
  }

  override def part1(input: Computer): String = {
    input.run().mkString(",")
  }

  // Adapted from https://github.com/lupari/aoc2024/blob/0725883755c3cc6fbdb5a0b8b1f3b6f402e5a575/src/main/scala/assignments/Day17.scala
  override def part2(input: Computer): Long = {
    val target = input.instructions.flatMap(_.toVector)

    def findA(ptr: Int, acc: Long): Option[Long] = {
      def helper(candidate: Int): Option[Long] = {
        val acc2 = acc * 8 + candidate
        if (input.copy(registers = input.registers.copy(a = acc2)).run() == target.drop(ptr)) {
          if (ptr == 0) Some(acc2) else findA(ptr - 1, acc2)
        } else {
          None
        }
      }

      (0 until 8).map(helper).collectFirst { case Some(a) => a }
    }

    findA(target.length - 1, 0L).getOrElse(-1)
  }
}
