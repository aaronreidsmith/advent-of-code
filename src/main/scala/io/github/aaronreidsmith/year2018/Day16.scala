package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.io.Source

object Day16 extends Solution {
  type I  = (List[Scenario], String)
  type O1 = Int
  type O2 = Int

  case class OpCode(id: Int, a: Int, b: Int, c: Int)
  case class Scenario(before: Map[Int, Int], opCode: OpCode, after: Map[Int, Int] = Map()) {
    lazy val instructions: Map[String, Map[Int, Int]] = Map(
      // Addition
      "addr" -> before.updated(opCode.c, before(opCode.a) + before(opCode.b)),
      "addi" -> before.updated(opCode.c, before(opCode.a) + opCode.b),
      // Multiplication
      "mulr" -> before.updated(opCode.c, before(opCode.a) * before(opCode.b)),
      "muli" -> before.updated(opCode.c, before(opCode.a) * opCode.b),
      // Bitwise AND
      "banr" -> before.updated(opCode.c, before(opCode.a) & before(opCode.b)),
      "bani" -> before.updated(opCode.c, before(opCode.a) & opCode.b),
      // Bitwise OR
      "borr" -> before.updated(opCode.c, before(opCode.a) | before(opCode.b)),
      "bori" -> before.updated(opCode.c, before(opCode.a) | opCode.b),
      // Assignment
      "setr" -> before.updated(opCode.c, before(opCode.a)),
      "seti" -> before.updated(opCode.c, opCode.a),
      // Greater-than testing
      "gtir" -> before.updated(opCode.c, if (opCode.a > before(opCode.b)) 1 else 0),
      "gtri" -> before.updated(opCode.c, if (before(opCode.a) > opCode.b) 1 else 0),
      "gtrr" -> before.updated(opCode.c, if (before(opCode.a) > before(opCode.b)) 1 else 0),
      // Equality testing
      "eqir" -> before.updated(opCode.c, if (opCode.a == before(opCode.b)) 1 else 0),
      "eqri" -> before.updated(opCode.c, if (before(opCode.a) == opCode.b) 1 else 0),
      "eqrr" -> before.updated(opCode.c, if (before(opCode.a) == before(opCode.b)) 1 else 0)
    )

    def all: Seq[Map[Int, Int]] = instructions.values.toSeq
  }

  private val before = """^Before:\s+\[(\d), (\d), (\d), (\d)]$""".r
  private val opcode = """^(\d+) (\d) (\d) (\d)$""".r
  private val after = """^After:\s+\[(\d), (\d), (\d), (\d)]$""".r
  extension (str: String) {
    def toAfter: Map[Int, Int] = str match {
      case after(r0, r1, r2, r3) => Map(0 -> r0.toInt, 1 -> r1.toInt, 2 -> r2.toInt, 3 -> r3.toInt)
      case _                     => throw new IllegalArgumentException
    }

    def toBefore: Map[Int, Int] = str match {
      case before(r0, r1, r2, r3) => Map(0 -> r0.toInt, 1 -> r1.toInt, 2 -> r2.toInt, 3 -> r3.toInt)
      case _                      => throw new IllegalArgumentException
    }

    def toOpCode: OpCode = str match {
      case opcode(oc, a, b, c) => OpCode(oc.toInt, a.toInt, b.toInt, c.toInt)
      case _                   => throw new IllegalArgumentException
    }
  }

  override def parseInput(file: Source): (List[Scenario], String) = {
    val Array(samples, program, _*) = file.mkString.trim.split("\n\n\n\n"): @unchecked
    val scenarios = samples.split("\n\n").toList.map { scenario =>
      val Array(before, opCode, after, _*) = scenario.split('\n'): @unchecked
      Scenario(before.toBefore, opCode.toOpCode, after.toAfter)
    }
    (scenarios, program)
  }

  override def part1(input: (List[Scenario], String)): Int = {
    val (scenarios, _) = input
    scenarios.count { scenario =>
      val valid = scenario.all.filter(_ == scenario.after)
      valid.length >= 3
    }
  }

  // Lil hacky because I didn't wanna refactor after part 1, lol
  override def part2(input: (List[Scenario], String)): Int = {
    def determineOpCodes(scenarios: Seq[Scenario]): Map[Int, String] = {
      val initial = (0 to 15).map(_ -> scenarios.head.instructions.keys.toSet).toMap
      scenarios
        .foldLeft(initial) { (acc, scenario) =>
          val opCode = scenario.opCode.id
          val candidates = scenario.instructions.collect {
            case (op, after) if after == scenario.after => op
          }.toSet
          val opCodes      = acc(opCode).intersect(candidates)
          val newOpCodeMap = acc.updated(opCode, opCodes)

          if (opCodes.size == 1) {
            newOpCodeMap.view.mapValues(op => if (op == opCodes) op else op -- opCodes).toMap
          } else {
            newOpCodeMap
          }
        }
        .view
        .mapValues(_.head)
        .toMap
    }

    val (scenarios, program) = input
    val opCodeMap            = determineOpCodes(scenarios)
    val programEntries       = program.split('\n')
    val initialOpCode        = programEntries.head.toOpCode
    val initialOp            = opCodeMap(initialOpCode.id)
    val partiallyInitialized = Scenario(Map(0 -> 0, 1 -> 0, 2 -> 0, 3 -> 0), initialOpCode)
    val initial              = partiallyInitialized.copy(after = partiallyInitialized.instructions(initialOp))
    programEntries.tail
      .foldLeft(initial) { (currentState, entry) =>
        val newBefore               = currentState.after
        val newOpCode               = entry.toOpCode
        val newOp                   = opCodeMap(newOpCode.id)
        val newPartiallyInitialized = Scenario(newBefore, newOpCode)
        newPartiallyInitialized.copy(after = newPartiallyInitialized.instructions(newOp))
      }
      .after(0)
  }
}
