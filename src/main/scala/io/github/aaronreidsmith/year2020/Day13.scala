package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

object Day13 {
  def main(args: Array[String]): Unit = {
    val Array(arrivalTimeRaw, busesRaw) = using("2020/day13.txt")(_.mkString.split('\n'))
    val arrivalTime                     = arrivalTimeRaw.toInt
    val part1 = busesRaw
      .split(',')
      .collect { case entry if entry != "x" => entry.toInt }
      .map(busId => Seq(busId, busId - arrivalTime % busId))
      .minBy(_.last)
      .product
    println(s"Part 1: $part1")
    val part2 = {
      val buses   = busesRaw.split(',').zipWithIndex.filterNot { case (key, _) => key == "x" }
      val times   = buses.map(_._1.toLong)
      val offsets = buses.map { case (key, index) => key.toLong - index }
      val N       = times.product
      // https://brilliant.org/wiki/chinese-remainder-theorem
      val x = offsets.zip(times).foldLeft(0L) {
        case (acc, (offset, departTime)) =>
          acc + offset * (N / departTime) * BigInt(N / departTime).modPow(-1, departTime).toLong
      }
      x % N
    }
    println(s"Part 2: $part2")
  }
}
