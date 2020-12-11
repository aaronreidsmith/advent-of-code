#!/usr/bin/env raku

sub traverse(@mountain, $i-increment, $j-increment, $trees-encountered = 0, $i = 0, $j = 0) {
    if $i > @mountain.elems - 1 {
        $trees-encountered;
    } else {
        my $tree-hit = @mountain[$i][$j] cmp '#' == Same ?? 1 !! 0;
        if $i == @mountain.elems - 1 {
            $trees-encountered + $tree-hit;
        } else {
            traverse(
                @mountain,
                $i-increment,
                $j-increment,
                $trees-encountered + $tree-hit,
                $i + $i-increment,
                $j + $j-increment
            );
        }
    }
}

sub MAIN($file, Bool :$p2 = False) {
    my @traversals = $p2 ?? (
        (1, 1),
        (1, 3),
        (1, 5),
        (1, 7),
        (2, 1)
    ) !! ((1, 3),); # Need the comma to make it a list of lists
    say [*] @traversals.map(-> ($i-increment, $j-increment) {
        traverse($file.IO.lines.map(-> $line { |$line.comb xx * }), $i-increment, $j-increment)
    });
}
