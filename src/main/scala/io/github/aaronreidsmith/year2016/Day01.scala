package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Direction, East, North, Point, Solution, South, West, using}

import scala.annotation.tailrec
import scala.io.Source

object Day01 extends Solution {
  type I  = List[Instruction]
  type O1 = Int
  type O2 = Int

  private[year2016] case class Instruction(direction: Char, steps: Int)
  private case class State(facing: Direction, position: Point, visited: Vector[Point] = Vector()) {
    @tailrec
    final def move(instruction: Instruction): State = instruction.direction match {
      case 'R' =>
        val (newPos, newVisited) = facing match {
          case North =>
            val newX       = position.x + instruction.steps
            val newVisited = (position.x until newX).map(Point(_, position.y))
            (position.copy(x = newX), visited ++ newVisited)
          case East =>
            val newY       = position.y - instruction.steps
            val newVisited = (position.y until newY by -1).map(Point(position.x, _))
            (position.copy(y = newY), visited ++ newVisited)
          case South =>
            val newX       = position.x - instruction.steps
            val newVisited = (position.x until newX by -1).map(Point(_, position.y))
            (position.copy(x = newX), visited ++ newVisited)
          case West =>
            val newY       = position.y + instruction.steps
            val newVisited = (position.y until newY).map(Point(position.x, _))
            (position.copy(y = newY), visited ++ newVisited)
        }
        State(facing.right, newPos, newVisited)
      case 'L' =>
        // Handle "L" by just turning the state around and swapping the instruction (rather than duplicate logic)
        this.copy(facing = facing.opposite).move(instruction.copy(direction = 'R'))
      case _ => throw new IllegalArgumentException
    }
  }

  private object State {
    def origin: State = State(North, Point.zero)
  }

  def run(): Unit = {
    println("Year 2016, Day 1")
    val input = using("2016/day01.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def parseInput(file: Source): List[Instruction] = {
    file.mkString
      .split(", ")
      .map(entry => Instruction(entry.head, entry.tail.toInt))
      .toList
  }

  override protected[year2016] def part1(input: List[Instruction]): Int = {
    traverse(input).position.manhattanDistance(Point.zero)
  }

  override protected[year2016] def part2(input: List[Instruction]): Int = {
    @tailrec
    def helper(points: Vector[Point], seen: Set[Point] = Set()): Int = {
      val head +: tail = points
      if (seen.contains(head)) {
        head.manhattanDistance(Point.zero)
      } else {
        helper(tail, seen + head)
      }
    }

    helper(traverse(input).visited)
  }

  private def traverse(instructions: List[Instruction]): State = {
    instructions.foldLeft(State.origin)((state, instruction) => state.move(instruction))
  }
}
