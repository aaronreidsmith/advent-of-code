#!/usr/bin/env perl6

use experimental :cached;

sub find-differences(@joltage-list, $pointer = 0, @differences = ()) {
    if $pointer == @joltage-list.elems - 1 {
        @differences;
    } else {
        my $a = @joltage-list[$pointer];
        my $b = @joltage-list[$pointer + 1];
        find-differences(@joltage-list, $pointer + 1, (|@differences, $b - $a));
    }
}

sub find-paths($current-joltage, @joltage-list) is cached {
    given $current-joltage {
        when * == @joltage-list.max { 1 }
        when * ∉ @joltage-list      { 0 }
        default {
            find-paths($current-joltage + 1, @joltage-list) +
            find-paths($current-joltage + 2, @joltage-list) +
            find-paths($current-joltage + 3, @joltage-list);
        }
    }
}

sub MAIN($file, Bool :$p2 = False) {
    my @adaptors = $file.IO.lines.map(*.Int).sort;
    my $device-joltage = @adaptors.max + 3;
    my @joltage-list = (0, |@adaptors, $device-joltage);
    if $p2 {
        say find-paths(0, @joltage-list);
    } else {
        my @differences = find-differences(@joltage-list);
        say @differences.grep(* == 1).elems * @differences.grep(* == 3).elems;
    }
}
