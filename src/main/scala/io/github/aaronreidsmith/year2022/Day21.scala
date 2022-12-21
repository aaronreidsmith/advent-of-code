package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution
import org.apache.commons.math3.complex.Complex

import scala.io.Source

// Adapted from https://old.reddit.com/r/adventofcode/comments/zrav4h/2022_day_21_solutions/j138aav/
object Day21 extends Solution(2022, 21) {
  type I  = Map[String, Node]
  type O1 = Long
  type O2 = Long

  private[year2022] case class Node(
      name: String,
      raw: String,
      var value: Option[Complex] = None,
      var lhs: Node = null,
      var op: String = null,
      var rhs: Node = null
  ) {
    def evaluate: Complex = value.getOrElse(op match {
      case "+" => lhs.evaluate.add(rhs.evaluate)
      case "-" => lhs.evaluate.subtract(rhs.evaluate)
      case "*" => lhs.evaluate.multiply(rhs.evaluate)
      case "/" => lhs.evaluate.divide(rhs.evaluate)
      case _   => throw new IllegalArgumentException
    })
  }

  override protected[year2022] def parseInput(file: Source): Map[String, Node] = {
    val nodes = file.getLines().foldLeft(Map.empty[String, Node]) { (acc, line) =>
      val Array(name, raw, _*) = line.split(": ")
      acc.updated(name, Node(name, raw))
    }

    def buildTree(nodeName: String): Unit = {
      val node = nodes(nodeName)
      if (node.raw.forall(_.isDigit)) {
        node.value = Some(new Complex(node.raw.toDouble))
      } else {
        val Array(lhs, op, rhs, _*) = node.raw.split(' ')
        node.lhs = nodes(lhs)
        node.op = op
        node.rhs = nodes(rhs)
        buildTree(lhs)
        buildTree(rhs)
      }
    }
    buildTree("root")

    nodes
  }

  override protected[year2022] def part1(input: Map[String, Node]): Long = input("root").evaluate.getReal.toLong

  override protected[year2022] def part2(input: Map[String, Node]): Long = {
    val root = input("root")
    input("humn").value = Some(new Complex(0, 1))
    val (solve, const) = root.rhs.evaluate match {
      case complex if complex.getImaginary != 0.0 => (root.rhs.evaluate, root.lhs.evaluate)
      case _                                      => (root.lhs.evaluate, root.rhs.evaluate)
    }
    const.subtract(solve.getReal).divide(solve.getImaginary).getReal.toLong
  }
}
