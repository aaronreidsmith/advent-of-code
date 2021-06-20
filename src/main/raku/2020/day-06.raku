#!/usr/bin/env raku

sub part-one($group) {
    (set $group.subst("\n", '', :g).comb).elems;
}

sub part-two($group) {
    [∩] $group.split("\n").map(-> $entry { set $entry.comb });
}

sub MAIN($file, Bool :$p2 = False) {
    say [+] $file.IO.lines(:nl-in("\n\n")).map($p2 ?? &part-two !! &part-one);
}
