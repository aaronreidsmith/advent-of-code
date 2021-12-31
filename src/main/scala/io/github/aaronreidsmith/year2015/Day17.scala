package io.github.aaronreidsmith.year2015

import io.github.aaronreidsmith.using

import java.util.concurrent.atomic.AtomicInteger
import scala.io.Source

object Day17 {
  private val hashCodeGenerator = new AtomicInteger

  // This whole class is only used so we can have duplicate container sizes when using `combinations`
  private[year2015] case class Container(size: Int) {
    private val _hashCode        = hashCodeGenerator.getAndIncrement()
    override def hashCode(): Int = _hashCode
  }

  def main(args: Array[String]): Unit = {
    val containers = using("2015/day17.txt")(_.getLines().toList.map(line => Container(line.toInt)))
    println(s"Part 1: ${part1(containers)}")
    println(s"Part 2: ${part2(containers)}")
  }

  private[year2015] def part1(containers: List[Container], liters: Int = 150): Int =
    getValidCombinations(containers, liters).length

  private[year2015] def part2(containers: List[Container], liters: Int = 150): Int = {
    val validCombinations = getValidCombinations(containers, liters)
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
