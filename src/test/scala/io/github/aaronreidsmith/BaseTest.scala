package io.github.aaronreidsmith

import org.scalatest.ParallelTestExecution
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

trait BaseTest extends AnyFlatSpec with Matchers with ParallelTestExecution {
  protected implicit class TestStringOps(str: String) {
    def asSource: Source = Source.fromString(str)
  }
}
