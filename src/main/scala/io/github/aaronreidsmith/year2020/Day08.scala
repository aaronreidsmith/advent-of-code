package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day08 extends Solution(2020, 8) {
  type I  = Vector[Cell]
  type O1 = Int
  type O2 = Int

  private sealed trait RunType
  private case object Terminal extends RunType
  private case object Infinite extends RunType

  private[year2020] case class Cell(operation: String, value: Int, visited: Boolean = false)

  override protected[year2020] def parseInput(file: Source): Vector[Cell] = {
    file.getLines().toVector.map { line =>
      val Array(operation, value, _*) = line.split(' ')
      Cell(operation, value.toInt)
    }
  }

  override protected[year2020] def part1(input: Vector[Cell]): Int = accumulate(input, part2 = false)._1

  override protected[year2020] def part2(input: Vector[Cell]): Int = {
    input.zipWithIndex
      .collect {
        case (cell, index) if cell.operation == "nop" =>
          accumulate(input.updated(index, cell.copy(operation = "jmp")), part2 = true)
        case (cell, index) if cell.operation == "jmp" =>
          accumulate(input.updated(index, cell.copy(operation = "nop")), part2 = true)
      }
      .collectFirst {
        case (accumulator, Terminal) => accumulator
      }
      .get
  }

  private def accumulate(initialInstructions: Vector[Cell], part2: Boolean): (Int, RunType) = {
    @tailrec
    def helper(instructions: Vector[Cell], pointer: Int = 0, accumulator: Int = 0): (Int, RunType) = {
      if (part2 && pointer == instructions.size - 1) {
        (accumulator, Terminal)
      } else if (instructions(pointer).visited) {
        (accumulator, Infinite)
      } else {
        val currentCell         = instructions(pointer)
        val updatedInstructions = instructions.updated(pointer, currentCell.copy(visited = true))
        currentCell.operation match {
          case "acc" => helper(updatedInstructions, pointer + 1, accumulator + currentCell.value)
          case "jmp" => helper(updatedInstructions, pointer + currentCell.value, accumulator)
          case "nop" => helper(updatedInstructions, pointer + 1, accumulator)
          case _     => throw new IllegalArgumentException
        }
      }
    }

    helper(initialInstructions)
  }
}
