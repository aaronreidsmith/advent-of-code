package io.github.aaronreidsmith.year2023

import io.github.aaronreidsmith.BaseTest

class Day21Test extends BaseTest {
  override val runPart2: Boolean = isCI

  val suite: Suite = Suite(
    fileInput,
    3651,
    607334325965751L
  )
}
