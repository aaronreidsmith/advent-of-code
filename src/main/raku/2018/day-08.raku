#!/usr/bin/env raku

sub MAIN($file, Bool :$p2 = False) {
  my @tree = $file.IO.words.map(*.Int);

  if $p2 {
    sub part-two() {
      my $child-sum = 0;
      my @child-totals;
      my $children = @tree.shift;
      my $metadata-count = @tree.shift;

      for ^$children {
        @child-totals.push(part-two());
      }

      if $children == 0 {
        for ^$metadata-count {
          $child-sum += @tree.shift;
        }
      } else {
        for ^$metadata-count {
          my $metadata = @tree.shift;
          $child-sum += @child-totals[$metadata - 1] unless $metadata > @child-totals.elems;
        }
      }

      $child-sum;
    }
    say part-two();
  } else {
    my $running-sum = 0;
    sub part-one() {
      my $children       = @tree.shift;
      my $metadata-count = @tree.shift;

      for ^$children {
        part-one();
      }
      for ^$metadata-count {
        $running-sum += @tree.shift;
      }
    }

    part-one();
    say $running-sum;
  }
}
