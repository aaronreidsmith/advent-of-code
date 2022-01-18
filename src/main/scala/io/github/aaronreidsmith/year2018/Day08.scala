package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.using

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day08 {
  def main(args: Array[String]): Unit = {
    val tree = using("2018/day08.txt")(parseInput)
    println(s"Part 1: ${part1(tree)}")
    println(s"Part 2: ${part2(tree)}")
  }

  private[year2018] def parseInput(file: Source): List[Int] = file.mkString.split(' ').toList.map(_.toInt)

  private[year2018] def part1(initialTree: List[Int]): Int = {
    val tree       = initialTree.toBuffer
    var runningSum = 0

    def helper(): Unit = {
      val children      = tree.remove(0)
      val metadataCount = tree.remove(0)

      (0 until children).foreach(_ => helper())
      (0 until metadataCount).foreach { _ =>
        runningSum += tree.remove(0)
      }
    }

    helper()
    runningSum
  }

  private[year2018] def part2(initialTree: List[Int]): Int = {
    val tree = initialTree.toBuffer
    def helper(): Int = {
      var childSum      = 0
      val childTotals   = ArrayBuffer.empty[Int]
      val children      = tree.remove(0)
      val metadataCount = tree.remove(0)

      (0 until children).foreach(_ => childTotals.append(helper()))

      if (children == 0) {
        (0 until metadataCount).foreach { _ =>
          childSum += tree.remove(0)
        }
      } else {
        (0 until metadataCount).foreach { _ =>
          val metadata = tree.remove(0)
          if (metadata <= childTotals.size) {
            childSum += childTotals(metadata - 1)
          }
        }
      }

      childSum
    }

    helper()
  }
}
