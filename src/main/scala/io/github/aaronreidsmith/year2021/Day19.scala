package io.github.aaronreidsmith.year2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

// Adapted from https://git.io/JD9nN
object Day19 {
  type Beacon = (Int, Int, Int)
  implicit class BeaconOps(beacon: Beacon) {
    val (x, y, z)                   = beacon
    def +(that: Beacon): Beacon     = (x + that.x, y + that.y, z + that.z)
    def -(that: Beacon): Beacon     = (x - that.x, y - that.y, z - that.z)
    def distance(that: Beacon): Int = (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
    def orientations: Seq[Beacon] = Seq(
      (x, y, z),
      (-y, x, z),
      (-x, -y, z),
      (y, -x, z),
      (-x, y, -z),
      (y, x, -z),
      (x, -y, -z),
      (-y, -x, -z),
      (-z, y, x),
      (-z, x, -y),
      (-z, -y, -x),
      (-z, -x, y),
      (z, y, -x),
      (z, x, y),
      (z, -y, x),
      (z, -x, -y),
      (x, -z, y),
      (-y, -z, x),
      (-x, -z, -y),
      (y, -z, -x),
      (x, z, -y),
      (-y, z, -x),
      (-x, z, y),
      (y, z, x)
    )
  }
  type Scanner = Set[Beacon]
  implicit class ScannerOps(scanner: Scanner) {
    def orientations: Seq[Scanner] = scanner.toSeq.map(_.orientations).transpose.map(_.toSet)
  }
  implicit class IteratorOps[T](it: Iterator[T]) {
    def headOption: Option[T]    = it.nextOption()
    def occurrences: Map[T, Int] = it.to(LazyList).groupMapReduce(identity)(_ => 1)(_ + _)
  }

  def main(args: Array[String]): Unit = {
    val input = Using.resource(Source.fromResource("2021/day19.txt")) { file =>
      val scanners = file.mkString.split("\n\n")
      scanners.toList.map { entry =>
        entry.split('\n').tail.foldLeft(Set.empty[Beacon]) { (acc, line) =>
          val Array(x, y, z, _*) = line.split(',')
          acc + ((x.toInt, y.toInt, z.toInt))
        }
      }
    }
    val (beacons, scannerPositions) = solution(input)
    val part1                       = beacons.size
    println(s"Part 1: $part1")
    val part2 = scannerPositions.combinations(2).foldLeft(Int.MinValue) {
      case (acc, Seq(point1, point2)) => acc.max(point1.distance(point2))
      case (acc, _)                   => acc
    }
    println(s"Part 2: $part2")
  }

  private def solution(input: Seq[Scanner]): (Scanner, Seq[Beacon]) = {
    def matchScanner(beacons: Scanner, scanner: Scanner): Option[(Scanner, Beacon)] = {
      for {
        orientedScanner <- scanner.orientations.iterator
        distances = {
          for {
            point1 <- beacons.iterator
            point2 <- orientedScanner.iterator
          } yield point1 - point2
        }.occurrences
        (distance, count) <- distances.iterator
        if count >= 12
      } yield (orientedScanner.map(_ + distance), distance)
    }.headOption

    @tailrec
    def helper(
        scanners: Seq[Scanner],
        beacons: Scanner,
        frontier: Scanner,
        scannerPositions: Set[Beacon]
    ): (Scanner, Seq[Beacon]) = {
      val newBeacons = beacons ++ frontier
      if (scanners.isEmpty) {
        (newBeacons, scannerPositions.toSeq)
      } else {
        val (matchedScanners, orientedScanners, matchedScannerPositions) = {
          for {
            scanner                       <- scanners
            (orientedScanner, scannerPos) <- matchScanner(frontier, scanner)
          } yield (scanner, orientedScanner, scannerPos)
        }.unzip3
        val newScanners         = scanners.filterNot(matchedScanners.contains)
        val newFrontier         = orientedScanners.reduceLeft(_ ++ _)
        val newScannerPositions = scannerPositions ++ matchedScannerPositions
        helper(newScanners, newBeacons, newFrontier, newScannerPositions)
      }
    }

    helper(input.tail, Set.empty, input.head, Set((0, 0, 0)))
  }
}
