package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day15 extends Solution {
  type I  = List[String]
  type O1 = Int
  type O2 = Int

  case class Lens(label: String, value: Int)

  override def parseInput(file: Source): List[String] = file.mkString.trim.split(',').toList

  override def part1(input: List[String]): Int = {
    input.foldLeft(0)(_ + hash(_))
  }

  override def part2(input: List[String]): Int = {
    def helper(instructions: List[String], boxes: Map[Int, List[Lens]]): Int = instructions match {
      case Nil =>
        boxes.foldLeft(0) {
          case (acc, (box, lenses)) =>
            acc + lenses.zipWithIndex.foldLeft(0) {
              case (innerAcc, (lens, i)) =>
                innerAcc + ((box + 1) * (i + 1) * lens.value)
            }
        }
      case head :: tail =>
        val updated = if (head.contains('=')) {
          val Array(label, value, _*) = head.split('='): @unchecked
          val box                     = hash(label)
          val existing                = boxes(box)
          // If a lens in that box has the same label, replace it
          val updatedBox = if (existing.exists(_.label == label)) {
            existing.map {
              case lens if lens.label == label => lens.copy(value = value.toInt)
              case other                       => other
            }
          } else {
            existing :+ Lens(label, value.toInt)
          }
          boxes.updated(box, updatedBox)
        } else {
          val label      = head.init
          val box        = hash(label)
          val updatedBox = boxes(box).filterNot(_.label == label)
          boxes.updated(box, updatedBox)
        }
        helper(tail, updated)
    }

    helper(input, (0 until 256).map(_ -> Nil).toMap)
  }

  private def hash(value: String): Int = value.foldLeft(0) { (acc, char) =>
    ((acc + char.toInt) * 17) % 256
  }
}
