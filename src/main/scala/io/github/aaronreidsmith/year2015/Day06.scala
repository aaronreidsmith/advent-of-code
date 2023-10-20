package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.{Point, Solution}

import scala.annotation.tailrec
import scala.io.Source

object Day06 extends Solution {
  type I  = List[Instruction]
  type O1 = Int
  type O2 = Int

  enum InstructionType {
    case TurnOn, TurnOff, Toggle
  }

  case class Instruction(instructionType: InstructionType, start: Point, end: Point)

  override def parseInput(file: Source): List[Instruction] = {
    val turnOn  = """^turn on (\d+),(\d+) through (\d+),(\d+)$""".r
    val turnOff = """^turn off (\d+),(\d+) through (\d+),(\d+)$""".r
    val toggle  = """^toggle (\d+),(\d+) through (\d+),(\d+)$""".r
    file
      .getLines()
      .foldLeft(Vector.empty[Instruction]) {
        case (acc, turnOn(x1, y1, x2, y2)) =>
          acc :+ Instruction(InstructionType.TurnOn, Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
        case (acc, turnOff(x1, y1, x2, y2)) =>
          acc :+ Instruction(InstructionType.TurnOff, Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
        case (acc, toggle(x1, y1, x2, y2)) =>
          acc :+ Instruction(InstructionType.Toggle, Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
        case (acc, _) => acc
      }
      .toList
  }

  override def part1(input: List[Instruction]): Int = {
    val lights = Array.fill(1000)(Array.fill(1000)(false))

    @tailrec
    def helper(instructions: List[Instruction]): Int = instructions match {
      case Nil => lights.foldLeft(0)(_ + _.count(_ == true))
      case current :: rest =>
        val Instruction(instructionType, start, end) = current
        instructionType match {
          case InstructionType.TurnOn =>
            for {
              x <- start.x to end.x
              y <- start.y to end.y
            } lights(x)(y) = true
          case InstructionType.TurnOff =>
            for {
              x <- start.x to end.x
              y <- start.y to end.y
            } lights(x)(y) = false
          case InstructionType.Toggle =>
            for {
              x <- start.x to end.x
              y <- start.y to end.y
            } {
              val current = lights(x)(y)
              lights(x)(y) = !current
            }
        }
        helper(rest)
    }

    helper(input)
  }

  override def part2(input: List[Instruction]): Int = {
    val lights = Array.fill(1000)(Array.fill(1000)(0))

    @tailrec
    def helper(instructions: List[Instruction]): Int = instructions match {
      case Nil => lights.foldLeft(0)(_ + _.sum)
      case current :: rest =>
        val Instruction(instructionType, start, end) = current
        instructionType match {
          case InstructionType.TurnOn =>
            for {
              x <- start.x to end.x
              y <- start.y to end.y
            } lights(x)(y) += 1
          case InstructionType.TurnOff =>
            for {
              x <- start.x to end.x
              y <- start.y to end.y
            } {
              val current = lights(x)(y)
              lights(x)(y) = if (current <= 0) 0 else current - 1
            }
          case InstructionType.Toggle =>
            for {
              x <- start.x to end.x
              y <- start.y to end.y
            } lights(x)(y) += 2
        }
        helper(rest)
    }

    helper(input)
  }
}
