package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.annotation.tailrec

object Day08 {
  private sealed trait RunType
  private case object Terminal extends RunType
  private case object Infinite extends RunType

  private case class Cell(operation: String, value: Int, visited: Boolean = false)

  def main(args: Array[String]): Unit = {
    val cells = using("2020/day08.txt") { file =>
      file.getLines().toVector.map { line =>
        val Array(operation, value, _*) = line.split(' ')
        Cell(operation, value.toInt)
      }
    }

    val (part1, _) = accumulate(cells, part2 = false)
    println(s"Part 1: $part1")

    val part2 = {
      val fixedInstructions = cells.zipWithIndex.collect {
        case (cell, index) if Seq("nop", "jmp").contains(cell.operation) =>
          cell.operation match {
            case "nop" => cells.updated(index, cell.copy(operation = "jmp"))
            case "jmp" => cells.updated(index, cell.copy(operation = "nop"))
            case _     => throw new IllegalArgumentException
          }
      }
      fixedInstructions
        .map(accumulate(_, part2 = true))
        .collectFirst { case (accumulator, Terminal) => accumulator }
        .get
    }
    println(s"Part 2: $part2")
  }

  @tailrec
  private def accumulate(
      instructions: Vector[Cell],
      part2: Boolean,
      pointer: Int = 0,
      accumulator: Int = 0
  ): (Int, RunType) = if (part2 && pointer == instructions.size - 1) {
    (accumulator, Terminal)
  } else if (instructions(pointer).visited) {
    (accumulator, Infinite)
  } else {
    val currentCell         = instructions(pointer)
    val updatedInstructions = instructions.updated(pointer, currentCell.copy(visited = true))
    currentCell.operation match {
      case "acc" => accumulate(updatedInstructions, part2, pointer + 1, accumulator + currentCell.value)
      case "jmp" => accumulate(updatedInstructions, part2, pointer + currentCell.value, accumulator)
      case "nop" => accumulate(updatedInstructions, part2, pointer + 1, accumulator)
      case _     => throw new IllegalArgumentException
    }
  }
}
