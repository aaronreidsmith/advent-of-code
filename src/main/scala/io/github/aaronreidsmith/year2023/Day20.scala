package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.Solution
import org.apache.commons.math3.util.ArithmeticUtils

import scala.collection.mutable
import scala.io.Source

// Adapted from https://github.com/Jadarma/advent-of-code-kotlin-solutions/blob/aa9cb761e4636233cdfdb458cd3055414f10871f/solutions/aockt/y2023/Y2023D20.kt
object Day20 extends Solution {
  type I  = SignalProcessor
  type O1 = Long
  type O2 = Long

  enum Pulse {
    case High, Low

    def opposite: Pulse = this match {
      case High => Low
      case Low  => High
    }
  }

  case class Signal(source: String, pulse: Pulse, destination: String)

  sealed trait Module {
    def name: String
    def sources: List[String]
    def destinations: List[String]

    def process(signal: Signal): Option[Pulse]

    def receiveAndEmit(signal: Signal): List[Signal] = process(signal) match {
      case None        => Nil
      case Some(pulse) => this.destinations.map(destination => Signal(name, pulse, destination))
    }
    def reset(): Unit = {}
  }

  case class Broadcast(name: String, sources: List[String], destinations: List[String]) extends Module {
    def process(signal: Signal): Option[Pulse] = Some(signal.pulse)
  }

  case class FlipFlop(name: String, sources: List[String], destinations: List[String]) extends Module {
    private var state: Pulse = Pulse.Low

    def process(signal: Signal): Option[Pulse] = signal.pulse match {
      case Pulse.High => None
      case Pulse.Low =>
        this.state = state.opposite
        Some(this.state)
    }

    override def reset(): Unit = {
      this.state = Pulse.Low
    }
  }

  case class Conjunction(name: String, sources: List[String], destinations: List[String]) extends Module {
    private val state = mutable.Map.from(sources.map(_ -> Pulse.Low))

    def process(signal: Signal): Option[Pulse] = {
      this.state.update(signal.source, signal.pulse)
      if (this.state.values.forall(_ == Pulse.High)) Some(Pulse.Low) else Some(Pulse.High)
    }

    override def reset(): Unit = {
      this.state.clear()
      sources.foreach(source => this.state.update(source, Pulse.Low))
    }
  }

  case class Debug(name: String, sources: List[String]) extends Module {
    val destinations: List[String]             = Nil
    def process(signal: Signal): Option[Pulse] = None
  }

  class SignalProcessor(modules: Map[String, Module]) {
    private var totalPresses = 0L

    private def pressButton(): Seq[Signal] = {
      this.totalPresses += 1
      val queue   = mutable.Queue(Signal("button", Pulse.Low, "broadcaster"))
      val signals = mutable.ArrayBuffer.empty[Signal]
      while (queue.nonEmpty) {
        val signal = queue.dequeue()
        signals.append(signal)
        queue.enqueueAll(this.modules(signal.destination).receiveAndEmit(signal))
      }
      signals.toSeq
    }

    private def reset(): Unit = {
      this.totalPresses = 0
      modules.values.foreach(_.reset())
    }

    def signalScoreAfter(iterations: Int): Long = {
      this.reset()

      var low  = 0L
      var high = 0L
      (0 until iterations).foreach { _ =>
        this.pressButton().foreach { signal =>
          signal.pulse match {
            case Pulse.Low  => low += 1
            case Pulse.High => high += 1
          }
        }
      }
      low * high
    }

    def estimatedPressesForRx(): Long = {
      this.reset()

      val rx     = modules("rx")
      val rxIn   = modules(rx.sources.head)
      val cycles = mutable.Map.from(rxIn.sources.map(_ -> -1L))
      while (cycles.values.exists(_ == -1L)) {
        pressButton()
          .filter { signal =>
            signal.destination == rxIn.name && signal.pulse == Pulse.High && cycles(signal.source) == -1
          }
          .foreach { signal =>
            cycles.update(signal.source, totalPresses)
          }
      }
      println(cycles)
      cycles.values.reduceLeft((a, b) => ArithmeticUtils.lcm(a, b))
    }
  }

  override def parseInput(file: Source): SignalProcessor = {
    val names        = mutable.Set.empty[String]
    val types        = mutable.Map.empty[String, String]
    val sources      = mutable.Map.empty[String, List[String]]
    val destinations = mutable.Map.empty[String, List[String]]

    val regex = """^([%&]?)([a-z]+) -> ((?:[a-z]+(?:, )?)+)$""".r
    file.getLines().foreach {
      case regex(moduleType, name, destinationsRaw) =>
        val outputs = destinationsRaw.split(", ").toList
        outputs.foreach { destination =>
          val existing = sources.getOrElse(destination, Nil)
          sources.update(destination, existing.appended(name))
        }
        names.add(name)
        names.addAll(outputs)
        destinations.update(name, outputs)
        types.update(name, moduleType)
    }

    val modules = names.foldLeft(Map.empty[String, Module]) { (acc, name) =>
      val sourceModules      = sources.getOrElse(name, mutable.ArrayBuffer.empty).toList
      val destinationModules = destinations.getOrElse(name, Nil)
      val module = types.get(name) match {
        case Some("")  => Broadcast(name, sourceModules, destinationModules)
        case Some("%") => FlipFlop(name, sourceModules, destinationModules)
        case Some("&") => Conjunction(name, sourceModules, destinationModules)
        case _         => Debug(name, sourceModules)
      }
      acc.updated(name, module)
    }

    new SignalProcessor(modules)
  }

  override def part1(input: SignalProcessor): Long = input.signalScoreAfter(1000)
  override def part2(input: SignalProcessor): Long = input.estimatedPressesForRx()
}
