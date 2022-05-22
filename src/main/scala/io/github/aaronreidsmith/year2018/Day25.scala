package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.using
import org.jgrapht.alg.connectivity.ConnectivityInspector
import org.jgrapht.graph.{DefaultEdge, DefaultUndirectedGraph}

import scala.io.Source

object Day25 {
  private[year2018] case class Star(w: Int, x: Int, y: Int, z: Int) {
    def distanceTo(that: Star): Int = {
      (this.w - that.w).abs + (this.x - that.x).abs + (this.y - that.y).abs + (this.z - that.z).abs
    }
  }

  def main(args: Array[String]): Unit = {
    val stars = using("2018/day25.txt")(parseInput)
    println(s"Part 1: ${part1(stars)}")
  }

  private[year2018] def parseInput(file: Source): List[Star] = {
    val star = """^(-?\d+),(-?\d+),(-?\d+),(-?\d+)$""".r
    file.getLines().foldLeft(List.empty[Star]) {
      case (acc, star(w, x, y, z)) => Star(w.toInt, x.toInt, y.toInt, z.toInt) :: acc
      case (acc, _)                => acc
    }
  }

  private[year2018] def part1(stars: List[Star]): Int = {
    val graph = new DefaultUndirectedGraph[Star, DefaultEdge](classOf[DefaultEdge])
    for {
      star      <- stars
      otherStar <- stars
      if star.distanceTo(otherStar) <= 3
    } {
      graph.addVertex(star)
      graph.addVertex(otherStar)
      graph.addEdge(star, otherStar)
    }

    new ConnectivityInspector(graph).connectedSets().size()
  }
}
