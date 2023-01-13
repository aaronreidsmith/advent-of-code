package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day24 extends Solution {
  private type Bridge = List[Component]
  type I              = Set[Component]
  type O1             = Int
  type O2             = Int

  case class Component(left: Int, right: Int) {
    val strength: Int = left + right

    def contains(value: Int): Boolean = left == value || right == value
    def other(i: Int): Int            = if (i == left) right else left
  }

  override def parseInput(file: Source): Set[Component] = {
    file.getLines().foldLeft(Set.empty[Component]) { (acc, line) =>
      val Array(left, right, _*) = line.split('/')
      acc + Component(left.toInt, right.toInt)
    }
  }

  override def part1(input: Set[Component]): Int = maxStrength(validBridges(input))

  override def part2(input: Set[Component]): Int = {
    val bridges        = validBridges(input).toSet
    val longestBridge  = bridges.map(_.length).max
    val longestBridges = bridges.filter(_.length == longestBridge).iterator
    maxStrength(longestBridges)
  }

  private def maxStrength(bridges: Iterator[Bridge]): Int = bridges.map(_.foldLeft(0)(_ + _.strength)).max

  private def validBridges(components: Set[Component], port: Int = 0): Iterator[Bridge] = {
    val portComponents = components.filter(_.contains(port))
    if (portComponents.nonEmpty) {
      for {
        component <- portComponents.iterator
        bridge    <- validBridges(components - component, component.other(port))
      } yield component :: bridge
    } else Iterator.single(Nil)
  }

}
