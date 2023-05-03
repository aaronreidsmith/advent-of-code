package io.github.aaronreidsmith

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source
import scala.reflect.runtime.universe

trait BaseTest extends AnyFlatSpec with Matchers with ParallelTestExecution {
  protected implicit class TestStringOps(str: String) {
    def asSource: Source = Source.fromString(str)
  }

  private val year          = getClass.getName.split("year")(1).take(4).toInt
  private val day           = getClass.getName.split("Day")(1).take(2).toInt
  private val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)

  protected val mainInstance: Solution = {
    val solution = runtimeMirror.moduleSymbol(Class.forName(f"io.github.aaronreidsmith.year$year.Day$day%02d$$"))
    runtimeMirror.reflectModule(solution).instance.asInstanceOf[Solution]
  }

  // May be overridden by tests
  protected val inputs: Seq[mainInstance.I] = {
    val input = using(f"$year/day$day%02d.txt")(mainInstance.parseInput)
    Seq(input)
  }

  // Implemented by tests
  protected val expected1: Seq[mainInstance.O1] = Seq()
  protected val expected2: Seq[mainInstance.O2] = Seq()

  s"Year $year Day $day part 1" should "work" in {
    inputs.zip(expected1).foreach { case (input, expected) => mainInstance.part1(input) shouldBe expected }
  }

  s"Year $year Day $day part 2" should "work" in {
    inputs.zip(expected2).foreach { case (input, expected) => mainInstance.part2(input) shouldBe expected }
  }
}
