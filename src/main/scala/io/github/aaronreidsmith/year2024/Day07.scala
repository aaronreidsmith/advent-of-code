package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day07 extends Solution {
  type I  = List[(Long, List[Long])]
  type O1 = Long
  type O2 = Long

  override def parseInput(file: Source): List[(Long, List[Long])] = {
    file.getLines().toList.map { line =>
      val split = line.split(":? ").toList.map(_.toLong)
      (split.head, split.tail)
    }
  }

  override def part1(input: List[(Long, List[Long])]): Long = solution(input, List(add, mul))
  override def part2(input: List[(Long, List[Long])]): Long = solution(input, List(add, mul, concat))

  private def solution(input: List[(Long, List[Long])], operators: List[(Long, Long) => Long]): Long = {
    input.foldLeft(0L) {
      case (acc, (target, args)) if canMake(target, args, operators) => acc + target
      case (acc, _)                                                  => acc
    }
  }

  private def canMake(target: Long, args: List[Long], operators: List[(Long, Long) => Long]): Boolean = {
    def helper(current: Long, remaining: List[Long]): Boolean = {
      if (current > target) {
        false
      } else {
        remaining match {
          case Nil         => current == target
          case arg :: rest => operators.exists(op => helper(op(current, arg), rest))
        }
      }
    }

    helper(args.head, args.tail)
  }

  private def mul(a: Long, b: Long): Long    = a * b
  private def add(a: Long, b: Long): Long    = a + b
  private def concat(a: Long, b: Long): Long = (a.toString + b.toString).toLong
}
