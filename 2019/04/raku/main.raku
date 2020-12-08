#!/usr/bin/env perl6

sub is-valid-password(Str(Int) $password, $part-two) {
    my $always-increasing = True;
    my $first = True;
    my $seen-double = False;
    my $previous-char;
    my $previous-num;
    for $password.comb -> $char {
        if $first {
            $first = False;
            $previous-char = $char;
            $previous-num = $char.Int;
        } else {
            if $char eq $previous-char {
                $seen-double = True;
            }
            if $char.Int < $previous-num {
                $always-increasing = False;
            }
            $previous-char = $char;
            $previous-num = $char.Int;
        }
    }
    $always-increasing && $seen-double && ?(any($password.comb.Bag.values) == 2);
}

sub MAIN($file, Bool :$p2 = False) {
    say $file.IO.slurp.split('-').map(*.Int).minmax.grep(&is-valid-password.assuming(*, $p2)).elems;
}
