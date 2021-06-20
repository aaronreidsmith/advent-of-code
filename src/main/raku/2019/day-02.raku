#!/usr/bin/env perl6

sub apply-intcode(@intcode, $pointer = 0) {
    given @intcode[$pointer] {
        when 1 .. 2 {
            # We need the value in the positions stored in the next indices
            my $int1 = @intcode[@intcode[$pointer + 1]];
            my $int2 = @intcode[@intcode[$pointer + 2]];

            # This one _does not_ ues the value stored at $pointer+3,
            # just the index of $pointer+3
            my $target-index = @intcode[$pointer + 3];

            when 1 {
                @intcode[$target-index] = $int1 + $int2;
                apply-intcode(@intcode, $pointer + 4);
            }
            when 2 {
                @intcode[$target-index] = $int1 * $int2;
                apply-intcode(@intcode, $pointer + 4);
            }
        }
        when 99 { @intcode.head }
        default { die "Not a valid intcode" }
    }
}

# https://adventofcode.com/2019/day/2
sub MAIN($file, Bool :$p2 = False) {
    my @intcode = $file.IO.slurp.split(',').map(*.Int);

    if $p2 {
        # From the problem:
        #
        # The inputs should still be provided to the program by replacing the
        # values at addresses 1 and 2, just like before. In this program, the
        # value placed in address 1 is called the noun, and the value placed
        # in address 2 is called the verb. Each of the two input values will
        # be between 0 and 99, inclusive.
        my $target = 19690720;
        for (^100) X (^100) -> @pair {
            my @intcode-copy = @intcode;
            @intcode-copy[1] = @pair[0];
            @intcode-copy[2] = @pair[1];
            last if apply-intcode(@intcode-copy) == $target;

            # From the problem:
            #
            # Find the input noun and verb that cause the program to produce
            # the output 19690720. What is 100 * noun + verb?
            #
            # This block runs when `last` is evaluated to true
            LAST {
                say (100 * @pair[0]) + @pair[1];
            }
        }
    } else {
        # From the problem:
        #
        # Once you have a working computer, the first step is to restore the
        # gravity assist program (your puzzle input) to the "1202 program alarm"
        # state it had just before the last computer caught fire. To do this,
        # before running the program, replace position 1 with the value 12 and
        # replace position 2 with the value 2.
        @intcode[1] = 12;
        @intcode[2] = 2;
        say apply-intcode(@intcode);
    }
}
