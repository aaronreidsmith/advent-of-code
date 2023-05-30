package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day13 extends Solution {
  type I  = Notes
  type O1 = Int
  type O2 = Long

  case class Notes(arrivalTime: Int, buses: Seq[String])

  override def parseInput(file: Source): Notes = {
    val Array(arrivalTimeRaw, busesRaw, _*) = file.mkString.trim.split('\n'): @unchecked
    Notes(arrivalTimeRaw.toInt, busesRaw.split(',').toSeq)
  }

  override def part1(input: Notes): Int = {
    val (earliestBusId, minutesWaiting) = input.buses
      .collect {
        case entry if entry != "x" =>
          val busId = entry.toInt
          (busId, busId - input.arrivalTime % busId)
      }
      .minBy(_._2)
    earliestBusId * minutesWaiting
  }

  override def part2(input: Notes): Long = {
    val buses   = input.buses.zipWithIndex.filterNot(_._1 == "x")
    val times   = buses.map(_._1.toLong)
    val offsets = buses.map((busId, index) => busId.toLong - index)
    // https://brilliant.org/wiki/chinese-remainder-theorem
    val N = times.product
    val x = offsets.zip(times).foldLeft(0L) {
      case (acc, (offset, departTime)) =>
        acc + offset * (N / departTime) * BigInt(N / departTime).modPow(-1, departTime).toLong
    }
    x % N
  }
}
