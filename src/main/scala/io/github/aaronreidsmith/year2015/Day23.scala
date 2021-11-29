package io.github.aaronreidsmith.year2015

import scala.annotation.tailrec
import scala.io.Source

object Day23 {
  private val hlf = "^hlf (.*)$".r
  private val tpl = "^tpl (.*)$".r
  private val inc = "^inc (.*)$".r
  private val jmp = "^jmp ([+-]\\d+)$".r
  private val jie = "^jie (.*?), ([+-]\\d+)$".r
  private val jio = "^jio (.*?), ([+-]\\d+)$".r

  protected[this] case class Computer(a: Int = 0, b: Int = 0)

  def main(args: Array[String]): Unit = {
    val input        = Source.fromResource("2015/day23.txt")
    val instructions = input.getLines().toVector
    input.close()

    @tailrec
    def solution(computer: Computer = Computer(), pointer: Int = 0): Int =
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
        solution(updatedComputer, updatedPointer)
      }

    println(s"Part 1: ${solution()}")
    println(s"Part 1: ${solution(Computer(a = 1))}")
  }
}
