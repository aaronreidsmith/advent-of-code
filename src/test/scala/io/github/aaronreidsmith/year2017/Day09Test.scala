package io.github.aaronreidsmith.year2017

import io.github.aaronreidsmith.BaseTest

class Day09Test extends BaseTest {
  val suite: Suite = Suite(
    Seq("{}", "{{{}}}", "{{},{}}", "{{{},{},{{}}}}", "{<a>,<a>,<a>,<a>}", "{{<ab>},{<ab>},{<ab>},{<ab>}}", "{{<!!>},{<!!>},{<!!>},{<!!>}}", "{{<a!>},{<a!>},{<a!>},{<ab>}}"),
    Seq(1, 6, 5, 16, 1, 9, 9, 3),
    Seq("<>", "<random characters>", "<<<<>", "<{!>}>", "<!!>", "<!!!>>", """<{o"i!a,<{i<a>""""),
    Seq(0, 17, 3, 2, 0, 0, 10)
  )
}
