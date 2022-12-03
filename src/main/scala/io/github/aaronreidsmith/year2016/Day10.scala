package io.github.aaronreidsmith.year2016

import io.github.aaronreidsmith.{Solution, using}

import scala.collection.mutable
import scala.io.Source

// TODO: Adapted from my Python solution, so v mutable
object Day10 extends Solution {
  type I  = (Map[Int, Vector[Int]], Map[Int, ((String, Int), (String, Int))])
  type O1 = Int
  type O2 = Int

  def run(): Unit = {
    println("Year 2016 Day 10")
    val input = using("2016/day10.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
    println()
  }

  override protected[year2016] def parseInput(
      file: Source
  ): (Map[Int, Vector[Int]], Map[Int, ((String, Int), (String, Int))]) = {
    val valueEntry = "^value (\\d+) goes to bot (\\d+)$".r
    val botEntry   = "^bot (\\d+) gives low to (bot|output) (\\d+) and high to (bot|output) (\\d+)$".r
    file.getLines().foldLeft((Map.empty[Int, Vector[Int]], Map.empty[Int, ((String, Int), (String, Int))])) {
      case ((botAcc, pipelineAcc), valueEntry(value, rawBotId)) =>
        val botId    = rawBotId.toInt
        val existing = botAcc.getOrElse(botId, Vector())
        (botAcc.updated(botId, existing :+ value.toInt), pipelineAcc)
      case ((botAcc, pipelineAcc), botEntry(rawBotId, outLoc1, lowId, outLoc2, highId)) =>
        val botId = rawBotId.toInt
        val out1  = lowId.toInt
        val out2  = highId.toInt
        (botAcc, pipelineAcc.updated(botId, ((outLoc1, out1), (outLoc2, out2))))
      case (acc, _) => acc
    }
  }

  override protected[year2016] def part1(
      input: (Map[Int, Vector[Int]], Map[Int, ((String, Int), (String, Int))])
  ): Int = {
    val (bots, pipeline) = input
    solution(bots, pipeline)._1
  }
  override protected[year2016] def part2(
      input: (Map[Int, Vector[Int]], Map[Int, ((String, Int), (String, Int))])
  ): Int = {
    val (bots, pipeline) = input
    solution(bots, pipeline)._2
  }

  private lazy val solution: (Map[Int, Vector[Int]], Map[Int, ((String, Int), (String, Int))]) => (Int, Int) =
    (initialBots, pipeline) => {
      val bots        = mutable.Map(initialBots.toSeq: _*).withDefaultValue(Vector())
      val output      = mutable.Map.empty[Int, Vector[Int]].withDefaultValue(Vector())
      var part1Answer = -1
      while (bots.nonEmpty) {
        bots.foreach {
          case (botId, chips) if chips.length == 2 =>
            val Vector(low, high, _*) = bots.remove(botId).getOrElse(Vector()).sorted
            if (low == 17 && high == 61) {
              part1Answer = botId
            }
            val ((outLoc1, out1), (outLoc2, out2)) = pipeline(botId)
            if (outLoc1 == "bot") {
              val existing = bots(out1)
              bots.update(out1, existing :+ low)
            } else {
              val existing = output(out1)
              output.update(out1, existing :+ low)
            }
            if (outLoc2 == "bot") {
              val existing = bots(out2)
              bots.update(out2, existing :+ high)
            } else {
              val existing = output(out2)
              output.update(out2, existing :+ high)
            }
          case _ => // Do nothing
        }
      }
      val part2Answer = Seq(0, 1, 2).foldLeft(1)((acc, location) => acc * output(location).head)

      (part1Answer, part2Answer)
    }
}
