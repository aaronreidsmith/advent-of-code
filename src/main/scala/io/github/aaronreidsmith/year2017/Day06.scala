package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.{Solution, using}

import scala.collection.mutable
import scala.io.Source

object Day06 extends Solution {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    val input = using("2017/day06.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  override protected[year2017] def parseInput(file: Source): List[Int] = file.mkString.split('\t').map(_.toInt).toList

  override protected[year2017] def part1(input: List[Int]): Int = {
    val seen       = mutable.Set.empty[List[Int]]
    var state      = input
    var iterations = 0
    while (!seen.contains(state)) {
      seen.add(state)
      iterations += 1
      state = iterate(state)
    }
    iterations
  }

  override protected[year2017] def part2(input: List[Int]): Int = {
    // Redo part 1 to get to infinite state
    val seen  = mutable.Set.empty[List[Int]]
    var state = input
    while (!seen.contains(state)) {
      seen.add(state)
      state = iterate(state)
    }

    val infiniteState = state
    state = iterate(state)
    var iterations = 1 // Start at 1 to make things easier
    while (state != infiniteState) {
      iterations += 1
      state = iterate(state)
    }
    iterations
  }

  private def iterate(state: List[Int]): List[Int] = {
    val (maxBank, maxIndex) = state.zipWithIndex.maxBy(_._1)

    val newState = state.toArray
    newState(maxIndex) = 0

    var currentIndex = maxIndex + 1
    (0 until maxBank).foreach { _ =>
      if (currentIndex > state.size - 1) currentIndex = 0
      newState(currentIndex) += 1
      currentIndex += 1
    }

    newState.toList
  }
}
