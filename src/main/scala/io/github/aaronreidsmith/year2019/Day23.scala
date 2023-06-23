package io.github.aaronreidsmith.year2019

import io.github.aaronreidsmith.Solution

import scala.annotation.tailrec
import scala.io.Source

object Day23 extends Solution {
  type I  = Network
  type O1 = Long
  type O2 = Long

  case class State(computer: IntCode, input: Vector[Long], output: Vector[Long])
  opaque type NAT     = Vector[Long]
  opaque type Network = Vector[State]
  extension (network: Network) {
    def step(nat: NAT): (Network, NAT) = {
      val nextNetwork = network.map { state =>
        val State(computer, input, output) = state
        val nextComputer = if (computer.input.nonEmpty) {
          computer.next
        } else if (input.nonEmpty) {
          computer.withInput(input: _*).next
        } else {
          computer.withInput(-1).next
        }
        val nextInput = if (computer.input.nonEmpty) input else Vector()
        val nextOutput = nextComputer.result match {
          case IntCode.Output(value) => output :+ value
          case _                     => output
        }
        State(nextComputer, nextInput, nextOutput)
      }

      nextNetwork.zipWithIndex.foldLeft((nextNetwork, nat)) {
        case ((currNetwork, currNat), (state, src)) =>
          if (state.output.length != 3) {
            (currNetwork, currNat)
          } else {
            val dest    = state.output.head.toInt
            val nextSrc = state.copy(output = Vector())
            if (dest == 255) {
              (currNetwork.updated(src, nextSrc), state.output.tail)
            } else {
              val nextDest = currNetwork(dest).copy(input = currNetwork(dest).input ++ state.output.tail)
              (currNetwork.updated(src, nextSrc).updated(dest, nextDest), currNat)
            }
          }
      }
    }
  }

  override def parseInput(file: Source): Network = {
    val intCode = IntCode(file)
    Vector.tabulate(50)(n => State(intCode, Vector(n), Vector()))
  }

  override def part1(input: Network): Long = {
    @tailrec
    def helper(network: Network, nat: NAT = Vector()): Long = network.step(nat) match {
      case (_, Seq(_, y))         => y
      case (nextNetwork, nextNat) => helper(nextNetwork, nextNat)
    }

    helper(input)
  }

  override def part2(input: Network): Long = {
    @tailrec
    def helper(network: Network, nat: NAT = Vector(), idleCount: Int = 0, previousIdleY: Option[Long] = None): Long = {
      val (nextNetwork, nextNat, nextIdleCount, nextIdleY) = if (idleCount < 700) {
        val (newNetwork, newNat) = network.step(nat)
        val active               = network.exists(state => state.input.nonEmpty || state.output.nonEmpty)
        val newIdleCount         = if (active) 0 else idleCount + 1
        (newNetwork, newNat, newIdleCount, None)
      } else {
        val newNetwork = network.updated(0, network(0).copy(input = nat))
        (newNetwork, nat, 0, Some(nat.last))
      }

      (previousIdleY, nextIdleY) match {
        case (Some(previous), Some(next)) if previous == next => next
        case _ => helper(nextNetwork, nextNat, nextIdleCount, nextIdleY.orElse(previousIdleY))
      }
    }

    helper(input)
  }
}
