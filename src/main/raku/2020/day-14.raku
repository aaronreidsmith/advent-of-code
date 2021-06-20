#!/usr/bin/env raku

sub find-all-masks(@zipped, $pointer = 0, @prefix = ()) {
    if $pointer == @zipped.elems {
        @prefix.join.parse-base(2);
    } else {
        my ($mask-digit, $digit) = @zipped[$pointer];
        given $mask-digit {
            when '0' { find-all-masks(@zipped, $pointer + 1, (|@prefix, $digit)) }
            when '1' { find-all-masks(@zipped, $pointer + 1, (|@prefix, $mask-digit)) }
            when 'X' {
                |(
                    find-all-masks(@zipped, $pointer + 1, (|@prefix, '0')),
                    find-all-masks(@zipped, $pointer + 1, (|@prefix, '1'))
                )
            }
        }
    }
}

sub apply-mask($mask, $num, Bool $part-two = False) {
    my @mask-list = $mask.comb;
    my @num-list  = $num.base(2).comb;

    while @num-list.elems < @mask-list.elems {
        @num-list.unshift(0);
    }

    if $part-two {
        find-all-masks(@mask-list Z @num-list);
    } else {
        my @masked = gather {
            for @mask-list Z @num-list -> ($mask-digit, $digit) {
                take $mask-digit eq 'X' ?? $digit !! $mask-digit;
            }
        }
        @masked.join.parse-base(2);
    }
}

sub extract-values($line) {
    my $address = $line.match(/\[(<digit>+)\]/)[0].Int;
    my $value = $line.split(' = ')[1].Int;
    ($address, $value)
}

sub MAIN($file, Bool :$p2 = False) {
    my $mask;
    my @mem;
    for $file.IO.lines -> $line {
        if $line.starts-with('mask') {
            $mask = $line.split(' = ')[1];
        } else {
            my ($address, $value) = extract-values($line);
            if $p2 {
                for apply-mask($mask, $address, $p2) -> $index {
                    @mem[$index] = $value;
                }
            } else {
                @mem[$address] = apply-mask($mask, $value);
            }
        }
    }
    say @mem.sum;
}
