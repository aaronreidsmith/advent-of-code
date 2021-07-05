package io.github.aaronreidsmith.year2015

import java.util.concurrent.atomic.AtomicInteger

import scala.io.Source

object Day17 {
  private val hashCodeGenerator = new AtomicInteger

  // This whole class is only used so we can have duplicate container sizes when using `combinations`
  protected[this] case class Container(size: Int) {
    private val _hashCode        = hashCodeGenerator.getAndIncrement()
    override def hashCode(): Int = _hashCode
  }

  def main(args: Array[String]): Unit = {
    val input      = Source.fromResource("2015/day17.txt")
    val containers = input.getLines().foldLeft(List.empty[Container])((acc, line) => acc :+ Container(line.toInt))
    input.close()

    val allCombinations = (1 to containers.length).foldLeft(List.empty[List[Container]]) { (acc, n) =>
      acc ++ containers.combinations(n)
    }
    val validContainers = allCombinations.filter(combination => (150 - combination.map(_.size).sum) == 0)

    val part1 = validContainers.length
    println(s"Part 1: $part1")

    val minContainers = validContainers.map(_.size).min
    val part2         = validContainers.count(_.size == minContainers)
    println(s"Part 2: $part2")
  }
}
