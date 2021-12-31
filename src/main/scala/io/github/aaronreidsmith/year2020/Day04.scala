package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith._

object Day04 {
  private val passportKeys         = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")
  private val northPoleCredentials = passportKeys - "cid"

  private val yearRegex  = "^(\\d{4})$".r
  private val heightIn   = "^(\\d+)in$".r
  private val heightCm   = "^(\\d+)cm$".r
  private val hairColor  = "^#[a-z0-9]{6}$".r
  private val passportId = "^(\\d{9})$".r

  def main(args: Array[String]): Unit = {
    val input = using("2020/day04.txt") { file =>
      file.mkString.split("\n\n").toList.map { entry =>
        entry
          .split("\\s")
          .map { pair =>
            val Array(key, value) = pair.split(':')
            key.trim -> value.trim
          }
          .toMap
      }
    }
    val part1 = input.count(isValid(_))
    println(s"Part 1: $part1")
    val part2 = input.count(isValid(_, checkValues = true))
    println(s"Part 2: $part2")
  }

  private def isValid(credentials: Map[String, String], checkValues: Boolean = false): Boolean = {
    val keys      = credentials.keySet
    val validKeys = passportKeys.subsetOf(keys) || northPoleCredentials.subsetOf(keys)
    if (validKeys && checkValues) {
      val validByr = credentials("byr") match {
        case yearRegex(year) => (1920 to 2002).contains(year.toInt)
        case _               => false
      }
      val validIyr = credentials("iyr") match {
        case yearRegex(year) => (2010 to 2020).contains(year.toInt)
        case _               => false
      }
      val validEyr = credentials("eyr") match {
        case yearRegex(year) => (2020 to 2030).contains(year.toInt)
        case _               => false
      }
      val validHgt = credentials("hgt") match {
        case heightIn(height) => (59 to 76).contains(height.toInt)
        case heightCm(height) => (150 to 193).contains(height.toInt)
        case _                => false
      }
      val validHcl = hairColor.findFirstIn(credentials("hcl")).isDefined
      val validEcl = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(credentials("ecl"))
      val validPid = passportId.findFirstIn(credentials("pid")).isDefined
      validByr && validIyr && validEyr && validHgt && validHcl && validEcl && validPid
    } else {
      validKeys
    }
  }
}
