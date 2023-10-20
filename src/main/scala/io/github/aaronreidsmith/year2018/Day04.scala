package io.github.aaronreidsmith.year2018

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source
import scala.math.Ordered.orderingToOrdered

// TODO: Copied from Raku solution, so quite a bit of mutability
object Day04 extends Solution {
  type I  = List[Entry]
  type O1 = Int
  type O2 = Int

  enum Memo {
    case FallsAsleep
    case WakesUp
    case BeginsShift(id: Int)
  }

  case class Timestamp(year: Int, month: Int, day: Int, hour: Int, minute: Int) extends Ordered[Timestamp] {
    def compare(that: Timestamp): Int = {
      (this.year, this.month, this.day, this.hour, this.minute)
        .compare((that.year, that.month, that.day, that.hour, that.minute))
    }
  }
  case class Entry(timestamp: Timestamp, memo: Memo)

  override def parseInput(file: Source): List[Entry] = {
    val entry       = """^\[(\d{4})-(\d{2})-(\d{2}) (\d{2}):(\d{2})] (.*)$""".r
    val beginsShift = """^Guard #(\d+) begins shift$""".r
    file
      .getLines()
      .toList
      .collect {
        case entry(year, month, day, hour, minute, rawMemo) =>
          val memo = rawMemo match {
            case beginsShift(id) => Memo.BeginsShift(id.toInt)
            case "falls asleep"  => Memo.FallsAsleep
            case "wakes up"      => Memo.WakesUp
            case _               => throw new IllegalArgumentException
          }
          Entry(Timestamp(year.toInt, month.toInt, day.toInt, hour.toInt, minute.toInt), memo)
      }
      .sortBy(_.timestamp)
  }

  override def part1(input: List[Entry]): Int = {
    val guardShifts                                = getGuardShifts(input)
    val (mostAsleepGuard, mostAsleepGuardSchedule) = guardShifts.maxBy((_, schedule) => schedule(-1))
    val mostAsleepMinute                           = mostAsleepGuardSchedule.removed(-1).maxBy(_._2)._1
    mostAsleepGuard * mostAsleepMinute
  }

  override def part2(input: List[Entry]): Int = {
    val guardShifts = getGuardShifts(input)
    val mostAsleepGuard = mutable.Map(
      "id"                       -> -1,
      "minute-most-asleep-key"   -> -1,
      "minute-most-asleep-value" -> -1
    )
    guardShifts.foreach { (id, minutesAsleep) =>
      val (minuteMostAsleepKey, minuteMostAsleepValue) = minutesAsleep.filterNot(_._1 == -1).maxBy(_._2)
      if (minuteMostAsleepValue > mostAsleepGuard("minute-most-asleep-value")) {
        mostAsleepGuard.update("id", id)
        mostAsleepGuard.update("minute-most-asleep-key", minuteMostAsleepKey)
        mostAsleepGuard.update("minute-most-asleep-value", minuteMostAsleepValue)
      }
    }
    mostAsleepGuard("id") * mostAsleepGuard("minute-most-asleep-key")
  }

  private def getGuardShifts(memos: List[Entry]): Map[Int, Map[Int, Int]] = {
    var currentGuard = Option.empty[Int]
    var currentStart = Option.empty[Timestamp]
    var currentEnd   = Option.empty[Timestamp]

    val guardShifts = mutable.Map.empty[Int, Map[Int, Int]]

    memos.foreach { entry =>
      entry.memo match {
        case Memo.BeginsShift(id) =>
          currentGuard = Some(id)
          currentStart = None
          currentEnd = None
        case Memo.FallsAsleep =>
          currentStart = Some(entry.timestamp)
          currentEnd = None
        case Memo.WakesUp =>
          currentEnd = Some(entry.timestamp)
          val startMinute = currentStart.fold(0)(_.minute)
          val endMinute   = currentEnd.fold(0)(_.minute)
          val guardId     = currentGuard.getOrElse(-1)
          if (!guardShifts.contains(guardId)) {
            val schedule = (-1 until 60).map(_ -> 0).toMap // -1 is key for total minutes asleep
            guardShifts.update(guardId, schedule)
          }
          (startMinute until endMinute).foreach { minute =>
            val existing  = guardShifts(guardId)
            val newMinute = existing(minute) + 1
            val newTotal  = existing(-1) + 1
            guardShifts.update(guardId, existing ++ Map(minute -> newMinute, -1 -> newTotal))
          }
          currentStart = None
          currentEnd = None
      }
    }

    guardShifts.toMap
  }
}
