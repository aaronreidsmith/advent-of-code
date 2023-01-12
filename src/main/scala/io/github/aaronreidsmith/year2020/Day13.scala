package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day13 extends Solution(2020, 13) {
  type I  = Notes
  type O1 = Int
  type O2 = Long

  private[year2020] case class Notes(arrivalTime: Int, buses: Seq[String])

  override protected[year2020] def parseInput(file: Source): Notes = {
    val Array(arrivalTimeRaw, busesRaw, _*) = file.mkString.trim.split('\n')
    Notes(arrivalTimeRaw.toInt, busesRaw.split(',').toSeq)
  }

  override protected[year2020] def part1(input: Notes): Int = {
    val (earliestBusId, minutesWaiting) = input.buses
      .collect {
        case entry if entry != "x" =>
          val busId = entry.toInt
          (busId, busId - input.arrivalTime % busId)
      }
      .minBy(_._2)
    earliestBusId * minutesWaiting
  }

  override protected[year2020] def part2(input: Notes): Long = {
    val buses   = input.buses.zipWithIndex.filterNot(_._1 == "x")
    val times   = buses.map(_._1.toLong)
    val offsets = buses.map { case (busId, index) => busId.toLong - index }
    // https://brilliant.org/wiki/chinese-remainder-theorem
    val N = times.product
    val x = offsets.zip(times).foldLeft(0L) {
      case (acc, (offset, departTime)) =>
        acc + offset * (N / departTime) * BigInt(N / departTime).modPow(-1, departTime).toLong
      case (acc, _) => acc
    }
    x % N
  }
}
