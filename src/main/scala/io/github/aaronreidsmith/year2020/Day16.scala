package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.using

import scala.collection.mutable
import scala.io.Source

object Day16 {
  def main(args: Array[String]): Unit = {
    val (trainInfo, myTicket, otherTickets) = using("2020/day16.txt")(parseInput)
    val allTickets                          = myTicket +: otherTickets
    println(s"Part 1: ${part1(trainInfo, allTickets)}")
    println(s"Part 2: ${part2(trainInfo, myTicket, otherTickets)}")
  }

  private[year2020] def parseInput(file: Source): (Map[String, Set[Int]], Vector[Int], Vector[Vector[Int]]) = {
    val Array(trainText, myTicketText, otherTicketText) = file.mkString.split("\n\n", 3)

    val trainInfo = trainText
      .split('\n')
      .map { line =>
        val Array(name, info) = line.split(": ", 2)
        val ranges = info.split(" or ").foldLeft(Set.empty[Int]) { (acc, range) =>
          val Array(start, end) = range.split("-", 2)
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

  private[year2020] def part1(trainInfo: Map[String, Set[Int]], tickets: Vector[Vector[Int]]): Int =
    tickets.foldLeft(0) {
      case (acc, ticket) if !isValid(ticket, trainInfo) => acc + findInvalidField(ticket, trainInfo)
      case (acc, _)                                     => acc
    }

  private[year2020] def part2(
      trainInfo: Map[String, Set[Int]],
      myTicket: Vector[Int],
      otherTickets: Vector[Vector[Int]]
  ): Long = {
    val validTickets = otherTickets.filter(isValid(_, trainInfo))
    val fields       = validTickets.transpose
    val fieldIndices = findFieldIndices(trainInfo, fields)
    fieldIndices.foldLeft(1L) {
      case (acc, (name, index)) if name.startsWith("departure") => acc * myTicket(index)
      case (acc, _)                                             => acc
    }
  }

  // Adapted from my Raku solution, so could be refactored to be more idiomatic
  private def findFieldIndices(trainInfo: Map[String, Set[Int]], fields: Vector[Vector[Int]]): Map[String, Int] = {
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
    ticket
      .collectFirst {
        case field if !fields.exists(_.contains(field)) => field
      }
      .getOrElse(0)
  }

  private def isValid(ticket: Vector[Int], trainInfo: Map[String, Set[Int]]): Boolean = {
    val fields = trainInfo.values.toVector
    ticket.forall(entry => fields.exists(_.contains(entry)))
  }
}
