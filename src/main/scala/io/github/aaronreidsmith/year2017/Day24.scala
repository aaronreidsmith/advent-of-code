package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day24 extends Solution(2017, 24) {
  private type Bridge = List[Component]
  type I              = Set[Component]
  type O1             = Int
  type O2             = Int

  protected[year2017] case class Component(left: Int, right: Int) {
    val strength: Int = left + right

    def contains(value: Int): Boolean = left == value || right == value
    def other(i: Int): Int            = if (i == left) right else left
  }

  override protected[year2017] def parseInput(file: Source): Set[Component] = {
    file.getLines().foldLeft(Set.empty[Component]) { (acc, line) =>
      val Array(left, right, _*) = line.split('/')
      acc + Component(left.toInt, right.toInt)
    }
  }

  override protected[year2017] def part1(input: Set[Component]): Int = maxStrength(validBridges(input))

  override protected[year2017] def part2(input: Set[Component]): Int = {
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
