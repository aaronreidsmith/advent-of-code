package io.github.aaronreidsmith

import io.github.aaronreidsmith.tags.{IgnoreOnCI, Part1Slow, Part2, Part2Slow}
import munit.FunSuite

import scala.io.Source
import scala.reflect.runtime.universe

// TODO: This file is a mess. Clean it up
trait BaseTest extends FunSuite {
  // Implemented by actual tests
  val suite: Suite

  // TODO: A lot of this is also in 'Solution'. Can we refactor?
  private val year          = getClass.getName.split("year")(1).take(4).toInt
  private val day           = getClass.getName.split("Day")(1).take(2).toInt
  private val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)

  // This is protected so we can expose its internal types
  protected val mainInstance: Solution = {
    val solution = runtimeMirror.moduleSymbol(Class.forName(f"io.github.aaronreidsmith.year$year.Day$day%02d$$"))
    runtimeMirror.reflectModule(solution).instance.asInstanceOf[Solution]
  }

  // Lazy because not all tests use the input from files
  protected lazy val fileInput: mainInstance.I = usingFile(f"$year/day$day%02d.txt")(mainInstance.parseInput)

  protected implicit class TestStringOps(str: String) {
    def parsed: mainInstance.I = mainInstance.parseInput(Source.fromString(str))
  }

  protected implicit class TestSeqOps(seq: Seq[String]) {
    def parsed: Seq[mainInstance.I] = seq.map(_.parsed)
  }

  // This is defined here instead of its own file to make the calling/implementing API a little easier, since we have
  // access to mainInstance's types here
  //
  // TODO: This gets a bit wonky if mainInstance.I is a collection since it has to be wrapped in an extra Seq
  protected trait Suite {
    def part1Input: Seq[mainInstance.I]
    def part1Expected: Seq[mainInstance.O1]
    def part2Input: Seq[mainInstance.I]
    def part2Expected: Seq[mainInstance.O2]
  }

  protected object Suite {
    private case class SuiteImpl(
        part1Input: Seq[mainInstance.I],
        part1Expected: Seq[mainInstance.O1],
        part2Input: Seq[mainInstance.I],
        part2Expected: Seq[mainInstance.O2]
    ) extends Suite

    // If we have just have 1 input for part 1 (i.e., a Day 25 situation)
    def apply(input: Any, expected1: Any): Suite = apply(Seq(input), Seq(expected1), Seq(), Seq())

    // If we have multiple inputs/outputs, but still just for part 1 (again, a Day 25 situation)
    def apply(inputs: Seq[Any], expected: Seq[Any]): Suite = apply(inputs, expected, Seq(), Seq())

    // If we have one shared input and just one expected output for each part
    def apply(input: Any, expected1: Any, expected2: Any): Suite = apply(
      Seq(input),
      Seq(expected1),
      Seq(input),
      Seq(expected2)
    )

    // If we have 2 separate input/output combos with just 1 expected input/output each
    def apply(input1: Any, expected1: Any, input2: Any, expected2: Any): Suite = apply(
      Seq(input1),
      Seq(expected1),
      Seq(input2),
      Seq(expected2)
    )

    // If we have a list of shared inputs and different outputs for each part
    def apply(inputs: Seq[Any], expected1: Seq[Any], expected2: Seq[Any]): Suite = apply(
      inputs,
      expected1,
      inputs,
      expected2
    )

    // Base case. All other apply methods will call this one eventually
    def apply(inputs1: Seq[Any], expected1: Seq[Any], inputs2: Seq[Any], expected2: Seq[Any]): Suite = {
      require(
        inputs1.length == expected1.length && inputs2.length == expected2.length,
        "Must have same number of inputs and outputs"
      )
      SuiteImpl(
        inputs1.map(_.asInstanceOf[mainInstance.I]),
        expected1.map(_.asInstanceOf[mainInstance.O1]),
        inputs2.map(_.asInstanceOf[mainInstance.I]),
        expected2.map(_.asInstanceOf[mainInstance.O2])
      )
    }
  }

  // TODO: We *always* tag our tests with all tags and then filter them in the implementing classes. Is there a better way?
  test(s"Year $year Day $day part 1".tag(Part1Slow).tag(IgnoreOnCI)) {
    suite.part1Input.zip(suite.part1Expected).foreach {
      case (input, expected) => assertEquals(mainInstance.part1(input), expected)
    }
  }

  test(s"Year $year Day $day part 2".tag(Part2).tag(Part2Slow).tag(IgnoreOnCI)) {
    suite.part2Input.zip(suite.part2Expected).foreach {
      case (input, expected) => assertEquals(mainInstance.part2(input), expected)
    }
  }
}
