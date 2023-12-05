package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

// Adapted from https://github.com/ondrsh/AdventOfCode2023/blob/4e2b50221b1f27f20371324a3e9348f22bb43bd3/src/main/kotlin/Day5.kt
object Day05 extends Solution {
  type I  = Almanac
  type O1 = Long
  type O2 = Long

  // Scala Ranges were too slow, since we don't need any range operations, this works fine
  case class LongRange(start: Long, end: Long)
  case class Almanac(seeds: List[Long], rangeMaps: List[Map[LongRange, LongRange]]) {
    private def mapRange(input: LongRange, mappings: Map[LongRange, LongRange]): List[LongRange] = {
      val output = mutable.ArrayBuffer.empty[LongRange]
      mappings.foreach {
        case (src, dst) =>
          val start = math.max(src.start, input.start)
          val end   = math.min(src.end, input.end)
          val shift = dst.start - src.start
          if (end >= start) {
            output.append(LongRange(start + shift, end + shift))
          }
      }

      if (output.isEmpty) {
        output.append(input)
      }

      if (output.size > 1) {
        val theirMin = mappings.keys.map(_.start).min
        val theirMax = mappings.keys.map(_.end).max
        if (input.start < theirMin) {
          output.append(LongRange(input.start, theirMin - 1))
        }
        if (input.end > theirMax) {
          output.append(LongRange(theirMax + 1, input.end))
        }
      }

      output.toList
    }

    private def solution(ranges: List[LongRange]): Long = ranges
      .flatMap { range =>
        rangeMaps.foldLeft(List(range)) { (acc, rangeMappings) =>
          acc.flatMap(mapRange(_, rangeMappings))
        }
      }
      .foldLeft(Long.MaxValue)((acc, range) => acc.min(range.start))

    def part1: Long = solution(seeds.map(seed => LongRange(seed, seed)))
    def part2: Long = solution(seeds.grouped(2).toList.collect {
      case start :: length :: Nil => LongRange(start, start + length)
    })
  }

  override def parseInput(file: Source): Almanac = {
    val blocks = file.mkString.trim.split("\n\n")
    val seeds  = blocks.head.split("\\s+").tail.map(_.toLong).toList
    val mappings = blocks.tail.toList.map { block =>
      block
        .split('\n')
        .tail
        .foldLeft(Map.empty[LongRange, LongRange]) { (acc, line) =>
          val Array(dst, src, length, _*) = line.split(' ').map(_.toLong): @unchecked
          acc.updated(LongRange(src, src + length - 1), LongRange(dst, dst + length - 1))
        }
    }
    Almanac(seeds, mappings)
  }

  override def part1(input: Almanac): Long = input.part1
  override def part2(input: Almanac): Long = input.part2
}
