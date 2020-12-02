#!/usr/bin/env perl6

grammar PasswordEntry {
    token TOP { ^(<digit>+)'-'(<digit>+) <.ws> (<[a..z]>)':' <.ws> (<[a..z]>+)$ }
}

class PartOneActions {
    method TOP($/) {
        my $range-start  = $/[0].Int;
        my $range-end    = $/[1].Int;
        my $target       = $/[2].Str;
        my $password     = $/[3].Str;
        my $target-count = $password.comb($target).elems;
        make so $range-start <= $target-count && $target-count <= $range-end;
    }
}

class PartTwoActions {
    method TOP($/) {
        my $position-one = $/[0].Int - 1;
        my $position-two = $/[1].Int - 1;
        my $target       = $/[2].Str;
        my @password     = $/[3].Str.comb;
        make so (
            (@password[$position-one] cmp $target) == Same
            xor
            (@password[$position-two] cmp $target) == Same
        );
    }
}

sub MAIN($file, Bool :$p2 = False) {
    my $actions = $p2 ?? PartTwoActions.new !! PartOneActions.new;
    say $file.IO.lines
          .map(-> $row { PasswordEntry.parse($row, :$actions).made })
          .grep(* == True)
          .elems;
}
