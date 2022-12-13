package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using
import ujson._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day13 {
  private[year2022] case class Packet(value: ujson.Value) extends Ordered[Packet] {
    def compare(that: Packet): Int = (value, that.value) match {
      case (a: Arr, b: Arr) =>
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
      case other                        => throw new IllegalArgumentException(other.toString())
    }
  }

  def main(args: Array[String]): Unit = {
    val input = using("2022/day13.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): List[Packet] = file.getLines().toList.collect {
    case line if line.nonEmpty => Packet(ujson.read(line))
  }

  protected[year2022] def part1(input: List[Packet]): Int = input.grouped(2).zipWithIndex.foldLeft(0) {
    case (acc, (Seq(a, b), index)) if a.compare(b) < 0 => acc + index + 1
    case (acc, _)                                      => acc
  }

  protected[year2022] def part2(input: List[Packet]): Int = {
    val dividerA = Packet(ujson.read("[[2]]"))
    val dividerB = Packet(ujson.read("[[6]]"))
    val sorted   = (dividerA :: dividerB :: input).sorted
    val indexA   = sorted.indexOf(dividerA) + 1
    val indexB   = sorted.indexOf(dividerB) + 1
    indexA * indexB
  }
}
