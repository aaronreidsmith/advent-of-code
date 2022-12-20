package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day20 extends Solution(2022, 20) {
  type I  = Vector[Int]
  type O1 = Long
  type O2 = Long

  private case class Node(value: Long, var prev: Node, var next: Node)

  override protected[year2022] def parseInput(file: Source): Vector[Int] = file.getLines().toVector.map(_.toInt)
  override protected[year2022] def part1(input: Vector[Int]): Long       = solution(input, 1, 1)
  override protected[year2022] def part2(input: Vector[Int]): Long       = solution(input, 811589153L, 10)

  private def buildNodes(input: Vector[Int], key: Long): Vector[Node] = {
    val nodes = input.map(n => Node(n * key, null, null))
    nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.prev = nodes((i - 1 + nodes.size) % nodes.size)
        node.next = nodes((i + 1) % nodes.size)
    }
    nodes
  }

  private def mix(nodes: Vector[Node]): Unit = nodes.foreach { node =>
    val rawSwaps = (node.value % (nodes.size - 1)).toInt
    val swaps    = if (rawSwaps >= 0) rawSwaps else rawSwaps + nodes.size - 1
    (0 until swaps).foreach { _ =>
      val (left, current, right, twoRight) = (node.prev, node, node.next, node.next.next)
      left.next = right
      current.prev = right
      current.next = twoRight
      right.prev = left
      right.next = current
      twoRight.prev = current
    }
  }

  private def skip(start: Node): Node = Iterator.iterate(start)(_.next).drop(1000).next()

  private def solution(input: Vector[Int], key: Long, rounds: Int): Long = {
    val nodes = buildNodes(input, key)
    (0 until rounds).foreach(_ => mix(nodes))
    val start = nodes.find(_.value == 0).get
    Iterator.iterate(start)(skip).slice(1, 4).foldLeft(0L)(_ + _.value)
  }
}
