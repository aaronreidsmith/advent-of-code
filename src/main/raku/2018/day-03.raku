#!/usr/bin/env raku

sub MAIN($file, Bool :$p2 = False) {
  my @squares = gather for $file.IO.lines -> $line {
    if $line ~~ m:s/^'#'(<digit>+) '@' (<digit>+)','(<digit>+)':' (<digit>+)'x'(<digit>+)$/ {
      take {id => $/[0].Int, left-indent => $/[1].Int, top-indent => $/[2].Int, width => $/[3].Int, height => $/[4].Int};
    }
  }
  my @fabric;
  for @squares -> %square {
    my $id               = %square<id>;
    my $x-start          = %square<left-indent>;
    my $x-end            = $x-start + %square<width> - 1;
    my $y-start          = %square<top-indent>;
    my $y-end            = $y-start + %square<height> - 1;
    for ($x-start..$x-end) X ($y-start..$y-end) -> ($column, $row) {
      my $existing-entry = @fabric[$row][$column];
      with $existing-entry {
        @fabric[$row][$column] ∪= $id;
      } else {
        @fabric[$row][$column] = Set.new($id);
      }
    }
  }
  my @overlaps = @fabric[*;*].grep(*.elems > 1);
  if $p2 {
    my @ids = @squares.map(*<id>);
    my $overlap-set;
    for @overlaps -> $overlap {
      $overlap-set ∪= $overlap;
    }
    say $overlap-set ⊖ @ids;
  } else {
    say @overlaps.elems;
  }
}
