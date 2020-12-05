#!/usr/bin/env perl6

sub binary-search(@list, @possible-rows, $lower-symbol) {
    if @possible-rows.elems == 1 {
        @possible-rows.head;
    } else {
        my ($first-item, @rest-of-list) := @list[0,1..*];
        my $half-way-point = @possible-rows.elems div 2;
        if $first-item eq $lower-symbol {
            binary-search(@rest-of-list, @possible-rows[^$half-way-point], $lower-symbol);
        } else {
            binary-search(@rest-of-list, @possible-rows[$half-way-point..*], $lower-symbol);
        }
    }
}

sub find-seat($boarding-pass) {
    my (@row-definition, @column-definition) := $boarding-pass.comb.rotor(7, :partial);
    my $row = binary-search(@row-definition, (^128), 'F');
    my $column = binary-search(@column-definition, (^8), 'L');
    ($row * 8) + $column;
}

sub MAIN($file, Bool :$p2 = False) {
    my @seats =  $file.IO.lines.map(&find-seat);
    my $max-seat = @seats.max;
    if $p2 {
        my $min-seat = @seats.min;
        my $all-seats = set ($min-seat..$max-seat);
        my @missing-seats = ($all-seats ⊖ set @seats).keys;
        say @missing-seats.head;
    } else {
        say $max-seat;
    }
}
