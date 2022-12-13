package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using
import ujson.{Arr, Num, Value, read}

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
      case (a: Arr, b: Num)       => Packet(a).compare(Packet(Arr(b)))
      case (_: Value, ujson.Null) => 1
      case (a: Num, b: Arr)       => Packet(Arr(a)).compare(Packet(b))
      case (ujson.Null, _: Value) => -1
      case (Num(a), Num(b))       => a.compare(b)
      case other                  => throw new IllegalArgumentException(other.toString())
    }
  }

  def main(args: Array[String]): Unit = {
    val input = using("2022/day13.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): List[Packet] = file.getLines().toList.collect {
    case line if line.nonEmpty => Packet(read(line))
  }

  protected[year2022] def part1(input: List[Packet]): Int = input.grouped(2).zipWithIndex.foldLeft(0) {
    case (acc, (Seq(a, b), index)) if a.compare(b) < 0 => acc + index + 1
    case (acc, _)                                      => acc
  }

  protected[year2022] def part2(input: List[Packet]): Int = {
    val decoderA = Packet(read("[[2]]"))
    val decoderB = Packet(read("[[6]]"))
    val sorted = (decoderA :: decoderB :: input).sorted
    val indexA = sorted.indexOf(decoderA) + 1
    val indexB = sorted.indexOf(decoderB) + 1
    indexA * indexB
  }
}
