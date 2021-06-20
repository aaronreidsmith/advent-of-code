#!/usr/bin/env raku

# This doesn't actually run... I wrote a Python solution (same logic) while this was still running and it worked
sub find-duplicate-frequency(@list, $pointer = 0, $current-freq = 0, $seen = Set[Int].new) {
    if $current-freq ∈ $seen {
        $current-freq;
    } else {
        my $next-frequency = $current-freq + @list[$pointer];
        my $next-pointer = $pointer == @list.elems - 1 ?? 0 !! $pointer + 1;
        find-duplicate-frequency(@list, $next-pointer, $next-frequency, $seen ∪ $current-freq);
    }
}

sub MAIN($file, Bool :$p2 = False) {
    my @numbers = $file.IO.lines.map(*.Int);
    if $p2 {
        say find-duplicate-frequency(@numbers);
    } else {
        say @numbers.sum;
    }
}
