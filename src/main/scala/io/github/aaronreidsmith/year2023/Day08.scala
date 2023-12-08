package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution
import org.apache.commons.math3.util.ArithmeticUtils

import scala.annotation.tailrec
import scala.io.Source

object Day08 extends Solution {
  type I  = (Iterator[Direction], Map[String, Node])
  type O1 = Long
  type O2 = Long

  enum Direction {
    case Left, Right
  }
  case class Node(left: String, right: String)

  override def parseInput(file: Source): (Iterator[Direction], Map[String, Node]) = {
    val Array(instructionsRaw, nodesRaw, _*) = file.mkString.trim.split("\n\n"): @unchecked

    val instructions = instructionsRaw.collect {
      case 'R' => Direction.Right
      case 'L' => Direction.Left
    }

    val regex = """^(\w{3}) = \((\w{3}), (\w{3})\)$""".r
    val nodes = nodesRaw.split("\n").toList.foldLeft(Map.empty[String, Node]) {
      case (acc, regex(value, left, right)) => acc.updated(value, Node(left, right))
      case (acc, _)                         => acc
    }

    (Iterator.continually(instructions).flatten, nodes)
  }

  override def part1(input: (Iterator[Direction], Map[String, Node])): Long = {
    solution(input, node => node == "ZZZ")("AAA")
  }

  override def part2(input: (Iterator[Direction], Map[String, Node])): Long = {
    val solver    = solution(input, node => node.endsWith("Z"))
    val solutions = input._2.keys.filter(_.endsWith("A")).toList.map(solver)
    solutions.foldLeft(1L)((acc, num) => ArithmeticUtils.lcm(acc, num))
  }

  private def solution(input: (Iterator[Direction], Map[String, Node]), endCondition: String => Boolean)(
      node: String
  ): Long = {
    val (instructions, nodeMap) = input

    @tailrec
    def helper(node: String, count: Long = 0L): Long = if (endCondition(node)) {
      count
    } else {
      val next = instructions.next() match {
        case Direction.Right => nodeMap(node).right
        case Direction.Left  => nodeMap(node).left
      }
      helper(next, count + 1)
    }

    helper(node)
  }
}
