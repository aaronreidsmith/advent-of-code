package io.github.aaronreidsmith

import scala.annotation.tailrec
import scala.reflect.runtime.universe
import scala.util.{Failure, Success, Try}

object AdventOfCode {
  private val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)
  private val years         = 2015 to 2023
  private val days          = 1 to 25

  def main(args: Array[String]): Unit = {
    val parsedArgs = parseArgs(args.toList)
    val yearToRun  = parsedArgs.get("year")
    val dayToRun   = parsedArgs.get("day")
    (yearToRun, dayToRun) match {
      case (Some(year), Some(day)) => runSolution(year, day)
      case (Some(year), None)      => days.foreach(runSolution(year, _))
      case (None, Some(day))       => years.foreach(runSolution(_, day))
      case (None, None) =>
        for {
          year <- years
          day  <- days
        } {
          runSolution(year, day)
        }
    }
  }

  @tailrec
  private def parseArgs(args: List[String], parsed: Map[String, Int] = Map()): Map[String, Int] = args match {
    case Nil => parsed
    case "--year" :: year :: tail =>
      val yearInt = year.toInt
      require(
        years.min <= yearInt && yearInt <= years.max,
        s"Year must be in the following range: ${years.min} <= year <= ${years.max}"
      )
      parseArgs(tail, parsed.updated("year", yearInt))
    case "--day" :: day :: tail =>
      val dayInt = day.toInt
      require(
        days.min <= dayInt && dayInt <= days.max,
        s"Day must be in the following range: ${years.min} <= day <= ${years.max}"
      )
      parseArgs(tail, parsed.updated("day", dayInt))
    case other => throw new IllegalArgumentException(s"Unknown argument(s) supplied: $other")
  }

  // Dynamically load and run a solution class
  private def runSolution(year: Int, day: Int): Unit = Try {
    val solution  = runtimeMirror.moduleSymbol(Class.forName(f"io.github.aaronreidsmith.year$year.Day$day%02d$$"))
    val runMethod = solution.typeSignature.members.filter(_.name.toString == "run").head.asMethod
    runtimeMirror.reflect(runtimeMirror.reflectModule(solution).instance).reflectMethod(runMethod)()
  } match {
    case Success(_) => // Do nothing
    case Failure(_) =>
      println(
        f"Year $year day $day has not been converted to a sub-type of Solution. Run it directly like so: sbt \"runMain io.github.aaronreidsmith.year$year.Day$day%02d\"\n"
      )
  }
}
