package io.github.aaronreidsmith.year2021

import io.github.aaronreidsmith.Solution
import io.github.aaronreidsmith.implicits.{headOption, occurrences}

import scala.annotation.tailrec
import scala.io.Source

// Adapted from https://git.io/JD9nN
object Day19 extends Solution {
  type I  = List[Scanner]
  type O1 = Int
  type O2 = Int

  case class Beacon(x: Int, y: Int, z: Int) {
    infix def +(that: Beacon): Beacon = copy(x + that.x, y + that.y, z + that.z)
    infix def -(that: Beacon): Beacon = copy(x - that.x, y - that.y, z - that.z)
    def distance(that: Beacon): Int   = (x - that.x).abs + (y - that.y).abs + (z - that.z).abs
    def orientations: Seq[Beacon] = Seq(
      this,
      copy(x = -y, y = x),
      copy(x = -x, y = -y),
      copy(x = y, y = -x),
      copy(x = -x, z = -z),
      copy(x = y, y = x, z = -z),
      copy(y = -y, z = -z),
      copy(x = -y, y = -x, z = -z),
      copy(x = -z, z = x),
      copy(x = -z, y = x, z = -y),
      copy(x = -z, y = -y, z = -x),
      copy(x = -z, y = -x, z = y),
      copy(x = z, z = -x),
      copy(x = z, y = x, z = y),
      copy(x = z, y = -y, z = x),
      copy(x = z, y = -x, z = -y),
      copy(y = -z, z = y),
      copy(x = -y, y = -z, z = x),
      copy(x = -x, y = -z, z = -y),
      copy(x = y, y = -z, z = -x),
      copy(y = z, z = -y),
      copy(x = -y, y = z, z = -x),
      copy(x = -x, y = z, z = y),
      copy(x = y, y = z, z = x)
    )
  }

  opaque type Scanner = Set[Beacon]
  extension (scanner: Scanner) {
    def orientations: Seq[Scanner] = scanner.toSeq.map(_.orientations).transpose.map(_.toSet)
  }

  override def parseInput(file: Source): List[Scanner] = file.mkString.trim.split("\n\n").toList.map { entry =>
    entry.split('\n').tail.foldLeft(Set.empty[Beacon]) { (acc, line) =>
      val Array(x, y, z, _*) = line.split(','): @unchecked
      acc + Beacon(x.toInt, y.toInt, z.toInt)
    }
  }

  override def part1(input: List[Scanner]): Int = solution(input)._1
  override def part2(input: List[Scanner]): Int = solution(input)._2

  // Orienting the beacons is expensive, so only want to do it once
  private var part1Answer = 0
  private var part2Answer = 0
  private var solved      = false
  private def solution(input: Seq[Scanner]): (Int, Int) = {
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

    if (!solved) {
      val (beacons, scannerPositions) = helper(input.tail, Set.empty, input.head, Set(Beacon(0, 0, 0)))
      part1Answer = beacons.size
      part2Answer = scannerPositions.combinations(2).foldLeft(Int.MinValue) {
        case (acc, Seq(point1, point2)) => acc.max(point1.distance(point2))
        case (acc, _)                   => acc
      }
      solved = true
    }
    (part1Answer, part2Answer)
  }
}
