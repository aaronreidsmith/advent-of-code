#!/usr/bin/env raku

sub adjacent($q, $r) {
    "{$q + 1}:$r",
    "$q:{$r + 1}",
    "{$q - 1}:{$r + 1}",
    "{$q - 1}:$r",
    "$q:{$r - 1}",
    "{$q + 1}:{$r - 1}"
}

sub MAIN($file, Bool :$p2 = False) {
    my $regex = /^(e|se|sw|w|nw|ne)+$/;
    my @instructions =
        $file.IO
        .lines
        .map(-> $line {
            $line.match($regex).map(*.Str).split(' ')
        });

    my $flipped = Set.new;
    for @instructions -> @instruction-set {
        my ($q, $r) = (0, 0);
        for @instruction-set -> $direction {
            given $direction {
                when 'e'  { $q += 1          }
                when 'se' { $r += 1          }
                when 'sw' { $q -= 1; $r += 1 }
                when 'w'  { $q -= 1          }
                when 'nw' { $r -= 1          }
                when 'ne' { $q += 1; $r -= 1 }
            }
        }
        my $tile = "$q:$r";
        if $tile ∈ $flipped {
            $flipped ⊖= $tile;
        } else {
            $flipped ∪= $tile;
        }
    }

    if $p2 {
        for (^100) {
            my $flipped-this-round = Set.new;
            my $unflipped-this-round = Set.new;
            my @q-range = $flipped.keys.map(*.split(':')[0].Int).minmax;
            my @r-range = $flipped.keys.map(*.split(':')[1].Int).minmax;
            for (@q-range.min - 1 .. @q-range.max + 1) -> $q {
                for (@r-range.min - 1 .. @r-range.max + 1) -> $r {
                    my $tile = "$q:$r";
                    my $adjacent-flipped = adjacent($q, $r).grep(* ∈ $flipped).elems;
                    if $tile ∈ $flipped {
                        if $adjacent-flipped == 0 || $adjacent-flipped > 2 {
                            $unflipped-this-round ∪= $tile;
                        }
                    } else {
                        if $adjacent-flipped == 2 {
                            $flipped-this-round ∪= $tile;
                        }
                    }
                }
            }
            $flipped ∪= $flipped-this-round;
            $flipped ⊖= $unflipped-this-round;
        }
    }
    say $flipped.elems;
}
