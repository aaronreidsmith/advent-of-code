package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.year2019.intcode.IntCode
import io.github.aaronreidsmith.year2019.intcode.util.IntCodeUtils

import scala.util.control.Breaks._

object Day07 extends IntCodeUtils {
  private case class Circuit(
      instructions: Map[Long, Long],
      a: Int,
      b: Int,
      c: Int,
      d: Int,
      e: Int,
      feedbackMode: Boolean
  ) {
    lazy val thrustSignal: Long = if (feedbackMode) {
      val suspendOnOutput = feedbackMode
      val aIntCode        = new IntCode(instructions, Seq(a.toLong, 0L), suspendOnOutput)
      val bIntCode        = new IntCode(instructions, Seq(b.toLong), suspendOnOutput)
      val cIntCode        = new IntCode(instructions, Seq(c.toLong), suspendOnOutput)
      val dIntCode        = new IntCode(instructions, Seq(d.toLong), suspendOnOutput)
      val eIntCode        = new IntCode(instructions, Seq(e.toLong), suspendOnOutput)

      // TODO: Use tail recursion or something
      def finished: Boolean = Seq(aIntCode, bIntCode, cIntCode, dIntCode, eIntCode).exists(_.isFinished)
      var aOutput: Option[Long] = None
      var bOutput: Option[Long] = None
      var cOutput: Option[Long] = None
      var dOutput: Option[Long] = None
      var eOutput: Option[Long] = None
      breakable {
        while (!finished) {
          aOutput = Some(aIntCode.run(eOutput.toSeq).getOutput.last)
          if (finished) break
          bOutput = Some(bIntCode.run(aOutput.toSeq).getOutput.last)
          if (finished) break
          cOutput = Some(cIntCode.run(bOutput.toSeq).getOutput.last)
          if (finished) break
          dOutput = Some(dIntCode.run(cOutput.toSeq).getOutput.last)
          if (finished) break
          eOutput = Some(eIntCode.run(dOutput.toSeq).getOutput.last)
        }
      }
      eOutput.get
    } else {
      val aOut = new IntCode(instructions, Seq(a.toLong, 0L)).run().getOutput.head
      val bOut = new IntCode(instructions, Seq(b.toLong, aOut)).run().getOutput.head
      val cOut = new IntCode(instructions, Seq(c.toLong, bOut)).run().getOutput.head
      val dOut = new IntCode(instructions, Seq(d.toLong, cOut)).run().getOutput.head
      new IntCode(instructions, Seq(e.toLong, dOut)).run().getOutput.head
    }
  }

  def main(args: Array[String]): Unit = {
    val instructions = makeInstructions("2019/day07.txt")
    val part1 = (0 to 4).permutations.map {
      case IndexedSeq(a, b, c, d, e) => Circuit(instructions, a, b, c, d, e, feedbackMode = false).thrustSignal
    }.max
    println(s"Part 1: $part1")

    val part2 = (5 to 9).permutations.map {
      case IndexedSeq(a, b, c, d, e) => Circuit(instructions, a, b, c, d, e, feedbackMode = true).thrustSignal
    }.max
    println(s"Part 2: $part2")
  }
}
