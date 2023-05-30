package io.github.aaronreidsmith.year2020

import io.github.aaronreidsmith.*

import scala.io.Source

object Day04 extends Solution {
  type I  = List[Map[String, String]]
  type O1 = Int
  type O2 = Int

  private val passportKeys         = Set("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid")
  private val northPoleCredentials = passportKeys - "cid"

  private val yearRegex  = """^(\d{4})$""".r
  private val heightIn   = """^(\d+)in$""".r
  private val heightCm   = """^(\d+)cm$""".r
  private val hairColor  = """^#[a-z0-9]{6}$""".r
  private val passportId = """^(\d{9})$""".r

  override def parseInput(file: Source): List[Map[String, String]] = {
    file.mkString.trim.split("\n\n").toList.map { entry =>
      entry
        .split("\\s")
        .foldLeft(Map.empty[String, String]) { (acc, pair) =>
          val Array(key, value, _*) = pair.split(':'): @unchecked
          acc + (key -> value)
        }
    }
  }

  override def part1(input: List[Map[String, String]]): Int = input.count(isValid(_))
  override def part2(input: List[Map[String, String]]): Int = input.count(isValid(_, checkValues = true))

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
