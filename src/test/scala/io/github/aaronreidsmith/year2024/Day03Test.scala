package io.github.aaronreidsmith.year2024

import io.github.aaronreidsmith.BaseTest

class Day03Test extends BaseTest {
  val suite: Suite = Suite(
    "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",
    161,
    "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))",
    48
  )
}
