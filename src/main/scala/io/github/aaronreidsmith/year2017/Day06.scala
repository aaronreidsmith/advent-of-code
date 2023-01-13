package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day06 extends Solution {
  type I  = List[Int]
  type O1 = Int
  type O2 = Int

  override def parseInput(file: Source): List[Int] = file.mkString.trim.split('\t').map(_.toInt).toList

  override def part1(input: List[Int]): Int = {
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

  override def part2(input: List[Int]): Int = {
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
