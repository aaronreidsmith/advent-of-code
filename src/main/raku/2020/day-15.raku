#!/usr/bin/env raku

# Only works for small $target-turn values
sub play-game-recursive(%numbers, $turn, $number, $target-turn) {
    if $turn == $target-turn {
        $number;
    } else {
        my $next-number = %numbers{$number}:exists ?? $turn - %numbers{$number} !! 0;
        play-game-recursive(
            %(|%numbers, |($number => $turn)),
            $turn + 1,
            $next-number,
            $target-turn
        );
    }
}

sub play-game-iterative(%numbers is copy, $target-turn) {
    my $last-item = %numbers.pairs.sort({ $^a.value cmp $^b.value })[*-1];
    for (%numbers.elems^..$target-turn) -> $turn {
        my $new-item = %numbers{$last-item}:exists ?? ($turn - 1) - %numbers{$last-item} !! 0;
        %numbers{$last-item} = $turn - 1;
        $last-item = $new-item;
    }
    $last-item;
}

sub MAIN($file, Bool :$p2 = False) {
    my %initial = $file.IO.slurp.split(',').kv.map(-> $key, $value { $value => $key + 1 } ).Hash;
    my $last-turn = $p2 ?? 30_000_000 !! 2020;
    say play-game-iterative(%initial, $last-turn);
}
