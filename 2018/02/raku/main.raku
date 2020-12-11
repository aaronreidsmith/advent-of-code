#!/usr/bin/env raku

sub MAIN($file, Bool :$p2 = False) {
    my @words = $file.IO.lines;
    if $p2 {
        say "Implement me";
    } else {
        my @dupes = @words.grep(-> $word { $word.comb.Bag.values ∋ 2 });
        my @trips = @words.grep(-> $word { $word.comb.Bag.values ∋ 3 });
        say @dupes.elems * @trips.elems;
    }
}
