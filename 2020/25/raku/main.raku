#!/usr/bin/env raku

sub MAIN($file) {
    my ($card-key, $door-key) = $file.IO.lines.map(*.Int);
    my $key = 1;
    my $target = 1;
    while $target != $door-key {
        $target = ($target * 7) % 20201227;
        $key = ($key * $card-key) % 20201227;
    }
    say $key;
}
