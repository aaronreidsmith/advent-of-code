package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.Solution

import scala.collection.mutable
import scala.io.Source

object Day16 extends Solution {
  type TrainInfo = Map[String, Set[Int]]
  type Ticket    = Vector[Int]
  type I         = (TrainInfo, Ticket, Vector[Ticket])
  type O1        = Int
  type O2        = Long

  override def parseInput(file: Source): (TrainInfo, Ticket, Vector[Ticket]) = {
    val Array(trainText, myTicketText, otherTicketText, _*) = file.mkString.trim.split("\n\n"): @unchecked

    val trainInfo = trainText
      .split('\n')
      .map { line =>
        val Array(name, info, _*) = line.split(": "): @unchecked
        val ranges = info.split(" or ").foldLeft(Set.empty[Int]) { (acc, range) =>
          val Array(start, end, _*) = range.split("-"): @unchecked
          acc ++ (start.toInt to end.toInt)
        }
        name -> ranges
      }
      .toMap

    val myTicket = myTicketText.split('\n').last.split(',').map(_.toInt).toVector
    val otherTickets = otherTicketText
      .split('\n')
      .tail
      .map(_.split(',').map(_.toInt).toVector)
      .toVector

    (trainInfo, myTicket, otherTickets)
  }

  override def part1(input: (TrainInfo, Ticket, Vector[Ticket])): Int = {
    val (trainInfo, myTicket, otherTickets) = input
    (myTicket +: otherTickets).foldLeft(0) {
      case (acc, ticket) if !isValid(ticket, trainInfo) => acc + findInvalidField(ticket, trainInfo)
      case (acc, _)                                     => acc
    }
  }

  override def part2(input: (TrainInfo, Ticket, Vector[Ticket])): Long = {
    val (trainInfo, myTicket, otherTickets) = input
    val validTickets                        = otherTickets.filter(isValid(_, trainInfo))
    val fields                              = validTickets.transpose
    val fieldIndices                        = findFieldIndices(trainInfo, fields)
    fieldIndices.foldLeft(1L) {
      case (acc, (name, index)) if name.startsWith("departure") => acc * myTicket(index)
      case (acc, _)                                             => acc
    }
  }

  // Adapted from my Raku solution, so could be refactored to be more idiomatic
  private def findFieldIndices(trainInfo: TrainInfo, fields: Vector[Vector[Int]]): Map[String, Int] = {
    val possibleFields = mutable.Map.empty[String, Vector[Int]].withDefaultValue(Vector())
    for {
      (field, index) <- fields.zipWithIndex
      (name, range)  <- trainInfo
      if field.forall(range.contains)
    } {
      val existing = possibleFields(name)
      possibleFields.update(name, existing :+ index)
    }

    val numberOfFields = possibleFields.size

    val finalFields = mutable.Map.empty[String, Int]
    while (finalFields.size != numberOfFields) {
      possibleFields.foreach {
        case (name, possibleIndices) =>
          if (possibleIndices.size == 1) {
            val index = possibleIndices.head
            finalFields.update(name, index)
            possibleFields.remove(name)
            possibleFields.foreach {
              case (nameToUpdate, indicesToUpdate) =>
                possibleFields.update(nameToUpdate, indicesToUpdate.filterNot(_ == index))
            }
          }
      }
    }

    finalFields.toMap
  }

  private def findInvalidField(ticket: Vector[Int], trainInfo: Map[String, Set[Int]]): Int = {
    val fields = trainInfo.values.toVector
    ticket.find(field => !fields.exists(_.contains(field))).getOrElse(0)
  }

  private def isValid(ticket: Vector[Int], trainInfo: Map[String, Set[Int]]): Boolean = {
    val fields = trainInfo.values.toVector
    ticket.forall(entry => fields.exists(_.contains(entry)))
  }
}
