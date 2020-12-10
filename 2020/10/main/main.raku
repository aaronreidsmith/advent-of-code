#!/usr/bin/env perl6

use experimental :cached;

sub find-differences(@joltage, $pointer = 0, @differences = ()) {
    if $pointer == @joltage.elems - 1 {
        @differences;
    } else {
        my $a = @joltage[$pointer];
        my $b = @joltage[$pointer + 1];
        find-differences(@joltage, $pointer + 1, (|@differences, $b - $a));
    }
}

sub find-paths($current-joltage, @joltage) is cached {
    given $current-joltage {
        when * == @joltage.max { 1 }
        when * ∉ @joltage      { 0 }
        default {
            find-paths($current-joltage + 1, @joltage) +
            find-paths($current-joltage + 2, @joltage) +
            find-paths($current-joltage + 3, @joltage);
        }
    }
}

sub MAIN($file, Bool :$p2 = False) {
    my @input-joltage = $file.IO.lines.map(*.Int).sort;
    my $device-joltage = @input-joltage.max + 3;
    my @joltage = (0, |@input-joltage, $device-joltage);
    if $p2 {
        say find-paths(0, @joltage);
    } else {
        my @differences = find-differences(@joltage);
        say @differences.grep(* == 1).elems * @differences.grep(* == 3).elems;
    }
}
