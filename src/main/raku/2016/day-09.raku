#!/usr/bin/env raku

sub part1(Str $input) returns Int {
  $input.subst(
    /
      '('(<digit>+)'x'(<digit>+)')' # (AxB)
      (\S+?)                        # Characters after group definition
      <?{ $2.chars == $0 }>         # With number of chars specified in group definition
    /,
    { $_[2] x $_[1] },              # Replace with characters repeated
    :g                              # Globally
  ).chars;
}

sub part2(Str $input) returns Int {
  # Find the first occurrence of (AxB)
  if ($input ~~ /'('(\d+)'x'(\d+)')' /) {
    # Count the to-be-expanded fragment and the rest of the string separately
    $/.from + part2($input.substr($/.to, $/[0])) * $/[1] + part2($input.substr($/.to + $/[0]));
  } else {
    $input.chars;
  }
}

sub MAIN(Str $file) {
  my $input = $file.IO.slurp.trim;
  say "Part 1: {part1($input)}";
  say "Part 2: {part2($input)}";
}
