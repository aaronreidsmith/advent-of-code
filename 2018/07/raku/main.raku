#!/usr/bin/env raku

sub calculate($num-workers, %deps is copy) {
  my $duration;
  my @ordered-steps = gather {
    my %work;
    while +%deps > 0 {
      my @next = ((%deps.keys ∖ [⊎] %deps.values).keys.sort ∖ %work.keys).keys.sort;

      my $free = $num-workers - +%work;
      for @next.splice(0, $free)  -> $s { %work{$s} = 60 + ($s.ord - 64); }

      my $timestep = %work.values.min;
      %work = %work.kv.map(* => * - $timestep);
      my @done = %work.grep(*.value == 0).hash.keys;

      %work{@done}:delete;
      %deps{@done}:delete;

      $duration += $timestep;
      take $_ for @done.sort;
    }
  }
  say "{@ordered-steps.join} in {$duration} seconds with {$num-workers} workers";
}

sub MAIN($file) {
  my %dependencies;
  for $file.IO.lines {
    my ($k, $v) = .comb(/ « <[A..Z]> » /);
    %dependencies{$k} //= SetHash.new;
    %dependencies{$v} //= SetHash.new;
    %dependencies{$k}{$v} = True;
  }

  calculate(1, %dependencies); # Part 1
  calculate(5, %dependencies); # Part 2
}
