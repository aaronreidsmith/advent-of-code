#!/usr/bin/env raku

sub MAIN($file, Bool :$p2 = False) {
    my @input = $file.IO.lines;
    if $p2 {
        # https://brilliant.org/wiki/chinese-remainder-theorem
        my @buses = @input[1].split(',').antipairs.grep(*.key ne 'x');

        my @times = @buses.map(*.key.Int);
        my @offsets = @buses.map(-> $pair { $pair.key - $pair.value });

        my $N = [*] @times;
        my @x = do for @offsets Z @times -> ($offset, $depart-time) {
            $offset * ($N div $depart-time) * expmod($N div $depart-time, -1, $depart-time);
        }

        say @x.sum % $N;
    } else {
        my @buses = @input[1].split(',').grep(* ne 'x').map(*.Int);
        my $arrival-time = @input[0].Int;
        say [*] @buses
                .map(-> $id { ($id, $id - $arrival-time % $id) })
                .min(*[1])
                .flat;
    }
}
