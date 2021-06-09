#!/usr/bin/env raku

# This file has the right logic, but is incredibly slow. I ran the same logic
# in Scala to get the answer

sub react(Str $polymer) returns Int {
  my $mutable-polymer = $polymer;
  my $index = 0;
  while $index < $mutable-polymer.comb.end {
    # Do this in the loop so we get a new list every time
    my @chars   = $mutable-polymer.comb;
    say @chars.elems;
    my $current = @chars[$index];
    my $next    = @chars[$index + 1];

    my $same-char     = $current.lc eq $next.lc;
    my $current-is-uc = $current.uc eq $current;
    my $next-is-uc    = $next.uc eq $next;

    if $same-char && ($current-is-uc xor $next-is-uc) {
      $mutable-polymer = @chars[^$index].join ~ @chars[$index + 1^..*].join;
      $index = 0;
    } else {
      $index++;
    }
  }
  $mutable-polymer.comb.elems;
}

sub react(Str $polymer, Str $filter-char) returns Int {
  my $mutable-polymer = $polymer.subst($filter-char, '').subst($filter-char.uc, '');
  my $index = 0;
  while $index < $mutable-polymer.comb.end {
    # Do this in the loop so we get a new list every time
    my @chars   = $mutable-polymer.comb;
    say @chars.elems;
    my $current = @chars[$index];
    my $next    = @chars[$index + 1];

    my $same-char     = $current.lc eq $next.lc;
    my $current-is-uc = $current.uc eq $current;
    my $next-is-uc    = $next.uc eq $next;

    if $same-char && ($current-is-uc xor $next-is-uc) {
      $mutable-polymer = @chars[^$index].join ~ @chars[$index + 1^..*].join;
      $index = 0;
    } else {
      $index++;
    }
  }
  $mutable-polymer.comb.elems;
}

sub MAIN($file, Bool :$p2 = False) {
  my $polymer = slurp($file);
  if $p2 {
    my %lengths = ('a'..'z').map: -> $char {
      $char => react($polymer, $char);
    }.Hash;
    say %lengths.values.min;
  } else {
    say react($polymer);
  }
}
