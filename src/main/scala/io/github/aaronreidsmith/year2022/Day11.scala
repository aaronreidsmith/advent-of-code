package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using

import scala.annotation.tailrec
import scala.collection.immutable.{Queue, SortedMap}
import scala.collection.mutable
import scala.io.Source

object Day11 {
  private[year2022] trait Monkey {
    val initialItems: Queue[Long] // Needed so we can "reset" the state between parts 1 and 2
    lazy val items: mutable.Queue[Long] = mutable.Queue.from(initialItems)

    def operate(old: Long, mod: Long): Long
    def test(item: Long): Int
    def reset(): Unit = {
      items.clear()
      items.enqueueAll(initialItems)
    }
  }

  def main(args: Array[String]): Unit = {
    val input = using("2022/day11.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 1: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): (SortedMap[Int, Monkey], Long) = {
    val monkey = """Monkey (\d):
                   |  Starting items: ([\d, ]+)
                   |  Operation: new = old (\+|\*) (\d+|old)
                   |  Test: divisible by (\d+)
                   |    If true: throw to monkey (\d)
                   |    If false: throw to monkey (\d)""".stripMargin.r
    file.mkString.trim.split("\n\n").foldLeft((SortedMap.empty[Int, Monkey], 1L)) {
      case ((monkeyAcc, modAcc), monkey(id, itemString, operator, opNum, testNum, throwIfTrue, throwIfFalse)) =>
        val newMonkey = new Monkey {
          val initialItems: Queue[Long] = Queue.from(itemString.split(", ").map(_.toLong))
          def operate(old: Long, mod: Long): Long = {
            val num = if (opNum == "old") old else opNum.toLong
            val newNum = operator match {
              case "+" => old + num
              case "*" => old * num
              case _   => throw new IllegalArgumentException
            }
            if (mod == 0L) newNum / 3 else newNum % mod
          }
          def test(item: Long): Int = {
            if (item % testNum.toLong == 0) throwIfTrue.toInt else throwIfFalse.toInt
          }
        }
        (monkeyAcc.updated(id.toInt, newMonkey), modAcc * testNum.toInt)
      case (acc, _) => acc
    }
  }

  protected[year2022] def part1(input: (SortedMap[Int, Monkey], Long)): Long = solution(input._1, 0L, 20)
  protected[year2022] def part2(input: (SortedMap[Int, Monkey], Long)): Long = {
    val (monkeys, mod) = input
    monkeys.values.foreach(_.reset())
    solution(monkeys, mod, 10_000)
  }

  protected[year2022] def solution(monkeys: SortedMap[Int, Monkey], mod: Long, iterations: Int): Long = {
    val counts = mutable.Map.from(monkeys.keys.map(_ -> 0L))

    @tailrec
    def helper(turn: Int): Long = if (turn >= iterations) {
      counts.values.toVector.sorted.takeRight(2).product
    } else {
      monkeys.foreach {
        case (monkeyId, monkey) =>
          while (monkey.items.nonEmpty) {
            val item     = monkey.items.dequeue()
            val operated = monkey.operate(item, mod)
            val newInt   = monkey.test(operated)
            monkeys(newInt).items.enqueue(operated)
            counts.update(monkeyId, counts(monkeyId) + 1)
          }
      }
      helper(turn + 1)
    }

    helper(0)
  }
}
