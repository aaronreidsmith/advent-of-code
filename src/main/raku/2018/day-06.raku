class Coord {
  has Int $.x;
  has Int $.y;
  has Int $.area is rw = 0;
  has Bool $.edge is rw = False;
}

sub MAIN($file) {
  my @coords = $file.IO.lines.map: {
    my ($x, $y) = .comb(/\d+/)>>.Int;
    Coord.new(:$x, :$y);
  };

  my ($min-x, $max-x) = @coords.min(*.x).x, @coords.max(*.x).x;
  my ($min-y, $max-y) = @coords.min(*.y).y, @coords.max(*.y).y;

  my $part2-area = 0;

  for $min-y .. $max-y -> $y {
    for $min-x .. $max-x -> $x {
      my @dist-pairs = @coords.map( { Pair.new(abs($x - .x) + abs($y - .y), $_) } );
      my @distances = @dist-pairs.map(*.key);
      my $min-dist = @distances.min;
      $part2-area += 1 if ([+] @distances) < 10_000;

      my @min = @dist-pairs.grep(*.key == $min-dist);
      if +@min == 1 {
        my $c = @min.head.value;
        $c.area += 1;
        $c.edge = True if $x == $min-x || $x == $max-x || $y == $min-y || $y == $max-y;
      }
    }
  }

  say @coords.grep(*.edge == False).max(*.area).area;
  say $part2-area;
}