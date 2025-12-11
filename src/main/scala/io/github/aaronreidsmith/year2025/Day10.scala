package io.github.aaronreidsmith.year2025

import com.microsoft.z3.{ArithExpr, Context, IntSort, Status}
import io.github.aaronreidsmith.Solution

import scala.io.Source
import scala.jdk.CollectionConverters.*

object Day10 extends Solution {
  type I  = Seq[Machine]
  type O1 = Int
  type O2 = Int

  case class Machine(target: Vector[Boolean], buttons: Seq[Seq[Int]], joltages: Vector[Int])

  override def parseInput(file: Source): Seq[Machine] = {
    val schematic = """^\[([.#]+)] (.*?) \{([\d,]+)}$""".r
    file
      .getLines()
      .toSeq
      .collect {
        case schematic(targetRaw, buttonsRaw, joltagesRaw) =>
          val target = targetRaw.toVector.map(char => if (char == '#') true else false)
          val buttons = buttonsRaw.split(' ').toSeq.map { tuple =>
            tuple.stripPrefix("(").stripSuffix(")").split(',').toSeq.map(_.toInt)
          }
          val joltages = joltagesRaw.stripPrefix("(").stripSuffix(")").split(',').toVector.map(_.toInt)
          Machine(target, buttons, joltages)
      }
  }

  override def part1(input: Seq[Machine]): Int = {
    input.foldLeft(0) { (acc, machine) =>
      val ctx = Context(Map("model" -> "true").asJava)
      val s   = ctx.mkOptimize()

      val buttonPresses = machine.buttons.indices.map(i => ctx.mkIntConst(s"x$i"))
      buttonPresses.foreach { presses =>
        s.Add(ctx.mkGe(presses, ctx.mkInt(0)))
        s.Add(ctx.mkLe(presses, ctx.mkInt(1)))
      }

      val totalPresses = buttonPresses.foldLeft[ArithExpr[IntSort]](ctx.mkInt(0))(ctx.mkAdd(_, _))
      s.MkMinimize(totalPresses)

      machine.target.indices.foreach { lightIdx =>
        val pressesAffectingLight = machine.buttons
          .zip(buttonPresses)
          .collect { case (pressAffectingLight, buttonPress) if pressAffectingLight.contains(lightIdx) => buttonPress }
          .foldLeft[ArithExpr[IntSort]](ctx.mkInt(0))(ctx.mkAdd(_, _))

        val targetValue = if (machine.target(lightIdx)) 1 else 0
        s.Add(ctx.mkEq(ctx.mkMod(pressesAffectingLight, ctx.mkInt(2)), ctx.mkInt(targetValue)))
      }

      assert(s.Check() == Status.SATISFIABLE)
      acc + s.getModel.evaluate(totalPresses, false).toString.toInt
    }
  }

  override def part2(input: Seq[Machine]): Int = {
    input.foldLeft(0) { (acc, machine) =>
      val ctx = Context(Map("model" -> "true").asJava)
      val s   = ctx.mkOptimize()

      val buttonPresses = machine.buttons.indices.map(i => ctx.mkIntConst(s"x$i"))
      buttonPresses.foreach(presses => s.Add(ctx.mkGe(presses, ctx.mkInt(0))))

      val totalPresses = buttonPresses.foldLeft[ArithExpr[IntSort]](ctx.mkInt(0))(ctx.mkAdd(_, _))
      s.MkMinimize(totalPresses)

      machine.buttons
        .zip(buttonPresses)
        .foldLeft(machine.joltages.map[ArithExpr[IntSort]](ctx.mkInt)) {
          case (acc, (button, presses)) =>
            button.foldLeft(acc)((inner, i) => inner.updated(i, ctx.mkSub(inner(i), presses)))
        }
        .foreach(joltageRemaining => s.Add(ctx.mkEq(joltageRemaining, ctx.mkInt(0))))

      assert(s.Check() == Status.SATISFIABLE)
      acc + s.getModel.evaluate(totalPresses, false).toString.toInt
    }
  }
}
