#!/usr/bin/env raku

sub part1($file) {
    my @ring;
    my ($current, $previous);
    for $file.IO.slurp.comb -> $number {
        $current //= $number.Int;
        with $previous {
            @ring[$previous] = $number.Int;
        }
        $previous = $number.Int;
    }
    @ring[$previous] = $current;

    for (^100) {
        my $pointer = $current;
        my %grabbed = %(
            0 => True,
            |(1 .. 3).map({
                $pointer = @ring[$pointer];
                $pointer => True;
            }).Hash
        );

        my $destination = $current - 1;
        while %grabbed{$destination}:exists {
            $destination = ($destination - 1) % 10;
        }

        (@ring[$current], @ring[$pointer], @ring[$destination]) = (@ring[$pointer], @ring[$destination], @ring[$current]);

        $current = @ring[$current];
    }

    $current = 1;
    my $output;
    while (($current = @ring[$current]) != 1) {
        $output ~= $current;
    }
    say $output;
}

sub part2($file) {
    my @ring;
    my ($current, $previous);
    for $file.IO.slurp.comb -> $number {
        $current //= $number.Int;
        with $previous {
            @ring[$previous] = $number.Int;
        }
        $previous = $number.Int;
    }

    for (10..1_000_000) -> $i {
        @ring[$previous] = $i;
        $previous = $i;
    }
    @ring[$previous] = $current;

    for (^10_000_000) {
        my $pointer = $current;
        my %grabbed = %(
            0 => True,
            |(1 .. 3).map({
                $pointer = @ring[$pointer];
                $pointer => True;
            }).Hash
        );

        my $destination = $current - 1;
        while %grabbed{$destination}:exists {
            $destination = ($destination - 1) % 1_000_001;
        }

        my $old             = @ring[$current];
        @ring[$current]     = @ring[$pointer];
        @ring[$pointer]     = @ring[$destination];
        @ring[$destination] = $old;

        $current = @ring[$current];
    }

    say @ring[1] * @ring[@ring[1]];
}

sub MAIN($file, Bool :$p2 = False) {
    $p2 ?? part2($file) !! part1($file);
}
