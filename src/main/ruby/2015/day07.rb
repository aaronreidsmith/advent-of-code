#!/usr/bin/env ruby

=begin
I am not smart enough to figure this out myself

All credit to: https://www.reddit.com/r/adventofcode/comments/3vr4m4/day_7_solutions/cxq07ki

To run, use this (to suppress warning):
ruby -W0 path/to/this/file path/to/input.txt

For part 2 replace this line in the input:
  Old: 1674 -> b
  New: 46065 -> b
=end

trans = {
  'AND'    => '&',
  'OR'     => '|',
  'NOT'    => '~',
  'LSHIFT' => '<<',
  'RSHIFT' => '>>'
}

# For part 2 replace this line in the input:
#   Old: 1674 -> b
#   New: 46065 -> b
p eval ARGF
  .read
  .gsub(Regexp.union(trans.keys), trans)
  .gsub(/(.+?) -> (\w+)/) { "%2s = #$1" % $2 }
  .upcase
  .split("\n")
  .sort
  .rotate
  .join(?;)
