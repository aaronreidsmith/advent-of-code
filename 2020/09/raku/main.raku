#!/usr/bin/env perl6

sub find-invalid(@list, $window-start = 0, $window-size = 25) {
    my $window-end = $window-start + $window-size - 1;
    my $target-number = @list[$window-end + 1];
    my @preamble-combinations = @list[$window-start..$window-end].combinations(2).map(*.sum);
    if $target-number ∈ @preamble-combinations {
        find-invalid(@list, $window-start + 1);
    } else {
        $target-number;
    }
}

sub find-contiguous-range(@list, $target, $start = 0, $end = 1) {
    my @range = @list[$start..$end];
    given @range.sum {
        when * < $target  { find-contiguous-range(@list, $target, $start, $end + 1) }
        when * == $target { @range }
        when * > $target  { find-contiguous-range(@list, $target, $start + 1, $start + 2) }
    }
}

sub MAIN($file, Bool :$p2 = False) {
    my @input = $file.IO.lines.map(*.Int);
    my $invalid = find-invalid(@input);
    if $p2 {
        my @contiguous-range = find-contiguous-range(@input.reverse, $invalid);
        say @contiguous-range.min + @contiguous-range.max;
    } else {
        say $invalid;
    }
}
