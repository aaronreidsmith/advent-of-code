package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day23 extends Solution {
  type I  = Vector[String]
  type O1 = Int
  type O2 = Int

  case class Computer(a: Int = 0, b: Int = 0)

  override def parseInput(file: Source): Vector[String] = file.getLines().toVector
  override def part1(instruction: Vector[String]): Int  = solution(instruction, Computer())
  override def part2(instructions: Vector[String]): Int = solution(instructions, Computer(a = 1))

  private def solution(instructions: Vector[String], initialComputer: Computer): Int = {
    val hlf = "^hlf (.*)$".r
    val tpl = "^tpl (.*)$".r
    val inc = "^inc (.*)$".r
    val jmp = """^jmp ([+-]\d+)$""".r
    val jie = """^jie (.*?), ([+-]\d+)$""".r
    val jio = """^jio (.*?), ([+-]\d+)$""".r

    @tailrec
    def helper(computer: Computer, pointer: Int = 0): Int =
      if (pointer < 0 || pointer >= instructions.length) {
        computer.b
      } else {
        val (updatedComputer, updatedPointer) = instructions(pointer) match {
          case hlf(reg) if reg == "a" => (computer.copy(a = computer.a / 2), pointer + 1)
          case hlf(reg) if reg == "b" => (computer.copy(b = computer.b / 2), pointer + 1)
          case tpl(reg) if reg == "a" => (computer.copy(a = computer.a * 3), pointer + 1)
          case tpl(reg) if reg == "b" => (computer.copy(b = computer.b * 3), pointer + 1)
          case inc(reg) if reg == "a" => (computer.copy(a = computer.a + 1), pointer + 1)
          case inc(reg) if reg == "b" => (computer.copy(b = computer.b + 1), pointer + 1)
          case jmp(offset)            => (computer, pointer + offset.toInt)
          case jie(reg, offset) if reg == "a" && computer.a % 2 == 0 => (computer, pointer + offset.toInt)
          case jie(reg, offset) if reg == "b" && computer.b % 2 == 0 => (computer, pointer + offset.toInt)
          case jie(_, _)                                         => (computer, pointer + 1)
          case jio(reg, offset) if reg == "a" && computer.a == 1 => (computer, pointer + offset.toInt)
          case jio(reg, offset) if reg == "b" && computer.b == 1 => (computer, pointer + offset.toInt)
          case jio(_, _)                                         => (computer, pointer + 1)
          case other                                             => throw new IllegalArgumentException(other)
        }
        helper(updatedComputer, updatedPointer)
      }

    helper(initialComputer)
  }
}
