package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day08 extends Solution {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[Int] = file.mkString.trim.split(' ').toList.map(_.toInt)

  override def part1(initialTree: List[Int]): Int = {
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

  override def part2(initialTree: List[Int]): Int = {
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
