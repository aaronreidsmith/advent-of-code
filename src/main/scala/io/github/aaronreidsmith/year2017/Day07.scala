package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day07 extends Solution {
  type I  = (Map[String, Node], Set[String])
  type O1 = String
  type O2 = Int

  case class Node(
      name: String,
      weight: Int,
      children: Seq[Node] = Seq(),
      childString: Option[String] = None
  )

  override def parseInput(file: Source): (Map[String, Node], Set[String]) = {
    val noChildren   = """^(\S+) \((\d+)\)$""".r
    val withChildren = """^(\S+) \((\d+)\) -> (.*)$""".r
    val allNodes = file.getLines().foldLeft(Map.empty[String, Node]) {
      case (acc, noChildren(name, weight)) => acc.updated(name, Node(name, weight.toInt))
      case (acc, withChildren(name, weight, childString)) =>
        acc.updated(name, Node(name, weight.toInt, childString = Some(childString)))
      case (acc, _) => acc
    }

    val childNodes = mutable.Set.empty[String]
    def getChildren(name: String): Node = {
      childNodes.add(name)

      val node     = allNodes(name)
      val children = node.childString.map(_.split(", ").map(getChildren)).getOrElse(Array.empty[Node])
      Node(node.name, node.weight, children.toSeq, node.childString)
    }

    val mappedNodes = allNodes.map {
      case (_, Node(name, weight, _, Some(children))) =>
        val mappedChildren = children.split(", ").map(getChildren).toSeq
        name -> Node(name, weight, mappedChildren, Some(children))
      case (name, node) => name -> node
    }

    (mappedNodes, childNodes.toSet)
  }

  override def part1(input: (Map[String, Node], Set[String])): String = {
    val (nodes, childNodes) = input
    nodes.keySet.diff(childNodes).head
  }

  override def part2(input: (Map[String, Node], Set[String])): Int = {
    def getWeight(node: Node): Int = node.weight + node.children.foldLeft(0)((acc, n) => acc + getWeight(n))

    def findOutlier(weights: Seq[Int]): Int = weights
      .groupBy(identity)
      .collectFirst {
        case (key, value) if value.size == 1 => key
      }
      .getOrElse(-1)

    @tailrec
    def traverse(node: Node, parentWeight: Option[Int] = None, parentSiblingWeight: Option[Int] = None): Int = {
      val weights       = node.children.map(child => child -> getWeight(child)).toMap
      val outlierWeight = findOutlier(weights.values.toList)
      weights.collectFirst {
        case (node, weight) if weight == outlierWeight => node
      } match {
        case Some(outlier) =>
          val nonOutlierWeight = weights
            .collectFirst {
              case (n, weight) if weight != outlierWeight => n
            }
            .map(getWeight)
          traverse(outlier, Some(getWeight(outlier)), nonOutlierWeight)
        case None =>
          val difference = math.abs(parentWeight.getOrElse(0) - parentSiblingWeight.getOrElse(0))
          node.weight - difference
      }
    }

    val (nodes, _) = input
    val root       = nodes(part1(input))
    traverse(root)
  }
}
