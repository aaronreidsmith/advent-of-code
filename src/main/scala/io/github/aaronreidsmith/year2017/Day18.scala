package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Day18 extends Solution {
  type I  = Vector[String]
  type O1 = Long
  type O2 = Int

  // Top-level so we only have to compile once
  private val snd = "^snd (.*)$".r
  private val set = "^set (.*) (.*)$".r
  private val add = "^add (.*) (.*)$".r
  private val mul = "^mul (.*) (.*)$".r
  private val mod = "^mod (.*) (.*)$".r
  private val rcv = "^rcv (.*)$".r
  private val jgz = "^jgz (.*) (.*)$".r

  override def parseInput(file: Source): Vector[String] = file.getLines().toVector

  override def part1(initialInstructions: Vector[String]): Long = {
    def getValue(registers: Map[String, Long], maybeValue: String): Long = Try(maybeValue.toInt) match {
      case Success(value) => value
      case Failure(_)     => registers.getOrElse(maybeValue, 0)
    }

    @tailrec
    def helper(
        instructions: Vector[String],
        registers: Map[String, Long] = Map(),
        recoveredFrequency: Option[Long] = None,
        position: Long = 0
    ): Long = if (position < 0 || position >= instructions.size) {
      recoveredFrequency.getOrElse(-1)
    } else {
      instructions(position.toInt) match {
        case snd(register) =>
          val frequency = getValue(registers, register)
          helper(
            instructions,
            if (registers.contains(register)) registers else registers + (register -> 0),
            Some(frequency),
            position + 1
          )
        case set(register, value) =>
          val newValue = getValue(registers, value)
          helper(instructions, registers + (register -> newValue), recoveredFrequency, position + 1)
        case add(register, value) =>
          val existingValue = getValue(registers, register)
          val increase      = getValue(registers, value)
          helper(instructions, registers + (register -> (existingValue + increase)), recoveredFrequency, position + 1)
        case mul(register, value) =>
          val existingValue = getValue(registers, register)
          val factor        = getValue(registers, value)
          helper(instructions, registers + (register -> (existingValue * factor)), recoveredFrequency, position + 1)
        case mod(register, value) =>
          val existingValue = getValue(registers, register)
          val divisor       = getValue(registers, value)
          helper(instructions, registers + (register -> (existingValue % divisor)), recoveredFrequency, position + 1)
        case rcv(register) =>
          val registerValue = getValue(registers, register)
          if (registerValue != 0) recoveredFrequency.getOrElse(-1)
          else helper(instructions, registers, recoveredFrequency, position + 1)
        case jgz(register, value) =>
          val registerValue = getValue(registers, register)
          val jumpValue     = getValue(registers, value)
          helper(
            instructions,
            registers,
            recoveredFrequency,
            if (registerValue > 0) position + jumpValue else position + 1
          )
        case _ => throw new IllegalArgumentException
      }
    }
    helper(initialInstructions)
  }

  // TODO: Copied from my Python solution, so v mutable
  override def part2(instructions: Vector[String]): Int = {
    class Program(pid: Long, var other: Program, instructions: Vector[String]) {
      val registers: mutable.Map[String, Long] = mutable.Map("p" -> pid).withDefaultValue(0)
      var ip: Long                             = 0
      val buffer: ArrayBuffer[Long]            = ArrayBuffer()
      var terminated: Boolean                  = false
      var blocked: Boolean                     = false
      var sent: Int                            = 0

      def next(): Unit = {
        if (terminated || ip < 0 || ip >= instructions.length) {
          terminated = true
          return
        }
        instructions(ip.toInt) match {
          case snd(register) =>
            other.buffer.append(get(register))
            other.blocked = false
            sent += 1
          case set(register, value) =>
            registers.update(register, get(value))
          case add(register, value) =>
            val existing = registers(register)
            registers.update(register, existing + get(value))
          case mul(register, value) =>
            val existing = registers(register)
            registers.update(register, existing * get(value))
          case mod(register, value) =>
            val existing = registers(register)
            registers.update(register, existing % get(value))
          case rcv(register) =>
            if (buffer.nonEmpty) {
              registers.update(register, buffer.remove(0))
            } else {
              blocked = true
              return
            }
          case jgz(register, value) =>
            if (get(register) > 0) {
              ip += get(value)
              return
            }
          case _ => throw new IllegalArgumentException
        }
        ip += 1
      }

      def get(v: String): Long = Try(v.toLong).getOrElse(registers(v))
    }

    val p0 = new Program(0, null, instructions)
    val p1 = new Program(1, p0, instructions)
    p0.other = p1

    while (!((p0.terminated || p0.blocked) && (p1.terminated || p1.blocked))) {
      p0.next()
      p1.next()
    }

    p1.sent
  }
}
