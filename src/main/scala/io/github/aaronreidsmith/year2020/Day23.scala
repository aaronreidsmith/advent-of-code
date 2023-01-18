package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source

// TODO: Copied directly from my Raku solution, so a lot of mutability
object Day23 extends Solution {
  type I  = Seq[Int]
  type O1 = String
  type O2 = Long

  override def parseInput(file: Source): Seq[Int] = file.mkString.trim.map(_.asDigit)

  override def part1(input: Seq[Int]): String = {
    val ring     = Array.fill(input.size + 1)(0)
    var current  = -1
    var previous = -1
    input.foreach { number =>
      current = if (current == -1) number else current
      if (previous != -1) {
        ring(previous) = number
      }
      previous = number
    }
    ring(previous) = current

    (0 until 100).foreach { _ =>
      var pointer = current
      val grabbed = (0 to 3).map {
        case 0 => 0
        case _ =>
          pointer = ring(pointer)
          pointer
      }.toSet

      var destination = current - 1
      while (grabbed.contains(destination)) {
        destination = if (destination == 0) ring.length - 1 else (destination - 1) % 10
      }

      val newCurrent     = ring(pointer)
      val newPointer     = ring(destination)
      val newDestination = ring(current)

      ring(current) = newCurrent
      ring(pointer) = newPointer
      ring(destination) = newDestination

      current = ring(current)
    }

    current = 1
    current = ring(current)
    val output = new StringBuilder
    while (current != 1) {
      output ++= current.toString
      current = ring(current)
    }
    output.result()
  }

  override def part2(input: Seq[Int]): Long = {
    val ring     = Array.fill(1_000_001)(0)
    var current  = -1
    var previous = -1
    input.foreach { number =>
      current = if (current == -1) number else current
      if (previous != -1) {
        ring(previous) = number
      }
      previous = number
    }

    (10 to 1_000_000).foreach { i =>
      ring(previous) = i
      previous = i
    }
    ring(previous) = current

    (0 until 10_000_000).foreach { _ =>
      var pointer = current
      val grabbed = (0 to 3).map {
        case 0 => 0
        case _ =>
          pointer = ring(pointer)
          pointer
      }.toSet

      var destination = current - 1
      while (grabbed.contains(destination)) {
        destination = if (destination == 0) ring.length - 1 else (destination - 1) % 1000001
      }

      val old = ring(current)
      ring(current) = ring(pointer)
      ring(pointer) = ring(destination)
      ring(destination) = old

      current = ring(current)
    }

    ring(1).toLong * ring(ring(1)).toLong
  }
}
