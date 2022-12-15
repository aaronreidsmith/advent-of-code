package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.Solution

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day13 extends Solution(2022, 13) {
  type I  = List[Packet]
  type O1 = Int
  type O2 = Int

  private[year2022] case class Packet(value: ujson.Value) extends Ordered[Packet] {
    def compare(that: Packet): Int = (value, that.value) match {
      case (a: ujson.Arr, b: ujson.Arr) =>
        a.value.zipAll(b.value, ujson.Null, ujson.Null).dropWhile {
          case (a, b) => Packet(a).compare(Packet(b)) == 0
        } match {
          case ArrayBuffer()           => 0
          case ArrayBuffer((a, b), _*) => Packet(a).compare(Packet(b))
        }
      case (a: ujson.Arr, b: ujson.Num) => Packet(a).compare(Packet(ujson.Arr(b)))
      case (_: ujson.Value, ujson.Null) => 1
      case (a: ujson.Num, b: ujson.Arr) => Packet(ujson.Arr(a)).compare(Packet(b))
      case (ujson.Null, _: ujson.Value) => -1
      case (ujson.Num(a), ujson.Num(b)) => a.compare(b)
      case _                            => throw new IllegalArgumentException
    }
  }

  override protected[year2022] def parseInput(file: Source): List[Packet] = file.getLines().toList.collect {
    case line if line.nonEmpty => Packet(ujson.read(line))
  }

  override protected[year2022] def part1(input: List[Packet]): Int = input.grouped(2).zipWithIndex.foldLeft(0) {
    case (acc, (Seq(a, b), index)) if a.compare(b) < 0 => acc + index + 1
    case (acc, _)                                      => acc
  }

  override protected[year2022] def part2(input: List[Packet]): Int = {
    val dividerA = Packet(ujson.read("[[2]]"))
    val dividerB = Packet(ujson.read("[[6]]"))
    val sorted   = (dividerA :: dividerB :: input).sorted
    val indexA   = sorted.indexOf(dividerA) + 1
    val indexB   = sorted.indexOf(dividerB) + 1
    indexA * indexB
  }
}
