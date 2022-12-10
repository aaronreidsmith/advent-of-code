package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.Solution

import java.util.concurrent.atomic.AtomicInteger
import scala.io.Source

object Day17 extends Solution(2015, 17) {
  type I  = List[Container]
  type O1 = Int
  type O2 = Int

  private val hashCodeGenerator = new AtomicInteger
  private val targetLiters      = if (isTest) 25 else 150

  // This whole class is only used so we can have duplicate container sizes when using `combinations`
  private[year2015] case class Container(size: Int) {
    private val _hashCode        = hashCodeGenerator.getAndIncrement()
    override def hashCode(): Int = _hashCode
  }

  override protected[year2015] def parseInput(file: Source): List[Container] = {
    file.getLines().toList.map(line => Container(line.toInt))
  }
  override protected[year2015] def part1(containers: List[Container]): Int =
    getValidCombinations(containers, targetLiters).length
  override protected[year2015] def part2(containers: List[Container]): Int = {
    val validCombinations = getValidCombinations(containers, targetLiters)
    val minContainers = validCombinations.foldLeft(Int.MaxValue) { (currentMin, combination) =>
      currentMin.min(combination.size)
    }
    validCombinations.count(_.size == minContainers)
  }

  private def getValidCombinations(containers: List[Container], liters: Int): List[List[Container]] = {
    val allCombinations = (1 to containers.length).foldLeft(List.empty[List[Container]]) { (acc, n) =>
      acc ++ containers.combinations(n)
    }
    allCombinations.filter(combination => (liters - combination.foldLeft(0)(_ + _.size)) == 0)
  }
}
