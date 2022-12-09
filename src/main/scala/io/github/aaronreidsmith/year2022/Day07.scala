package io.github.aaronreidsmith.year2022

import io.github.aaronreidsmith.using

import scala.collection.mutable
import scala.io.Source

object Day07 {
  def main(args: Array[String]): Unit = {
    val input = using("2022/day07.txt")(parseInput)
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2: ${part2(input)}")
  }

  protected[year2022] def parseInput(file: Source): (Set[String], Map[String, Int]) = {
    // Output variables
    val structure   = mutable.Map.empty[String, Int]
    val directories = mutable.Set("/")

    // Regex variables; we don't care about 'ls'
    val cdRegex   = """^\$ cd (\S+)$""".r
    val dirRegex  = """^dir (\S+)$""".r
    val fileRegex = """^(\d+) (\S+)$""".r

    // Skip first line for ease
    file.getLines().drop(1).foldLeft("") {
      case (currentDir, cdRegex(directory)) =>
        if (directory == "..") currentDir.split('/').init.mkString("/") else s"$currentDir/$directory"
      case (currentDir, dirRegex(name)) =>
        directories.add(s"$currentDir/$name")
        currentDir
      case (currentDir, fileRegex(size, name)) =>
        structure.update(s"$currentDir/$name", size.toInt)
        currentDir
      case (currentDir, _) => currentDir
    }

    (directories.toSet, structure.toMap)
  }

  protected[year2022] def part1(input: (Set[String], Map[String, Int])): Int = {
    val (directories, files) = input
    directories.foldLeft(0) { (acc, directory) =>
      val size = directorySize(files, directory)
      if (size <= 100_000) acc + size else acc
    }
  }

  protected[year2022] def part2(input: (Set[String], Map[String, Int])): Int = {
    val (directories, files) = input

    val totalSpace     = 70000000 // Given
    val spaceNeeded    = 30000000 // Given
    val spaceAvailable = totalSpace - directorySize(files, "/")

    directories.foldLeft(Int.MaxValue) { (currentMin, directory) =>
      val size = directorySize(files, directory)
      if (spaceAvailable + size >= spaceNeeded && size < currentMin) size else currentMin
    }
  }

  private def directorySize(files: Map[String, Int], directory: String): Int = files.foldLeft(0) {
    case (acc, (path, fileSize)) if path.startsWith(directory) => acc + fileSize
    case (acc, _)                                              => acc
  }
}