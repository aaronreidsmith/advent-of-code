#!/usr/bin/env perl6

use Tuple; # Needed to create a set of lists (tuples)

sub traverse(@directions, @position = (0, 0), $path-length = 0) {
    my ($x, $y) = @position;
    my (@new-position, @spaces-passed, @path-length) := gather {
        given @directions.head {
            when /U(<digit>+)/ {
                my $new-y = $y + $/[0].Int;
                take (
                    ($x, $new-y),
                    ($y...$new-y).map(-> $y-passed { ($x, $y-passed) }),
                    $path-length..^$path-length + $/[0].Int
                )
            }
            when /D(<digit>+)/ {
                my $new-y = $y - $/[0].Int;
                take (
                    ($x, $new-y),
                    ($y...$new-y).map(-> $y-passed { ($x, $y-passed) }),
                    $path-length..^$path-length + $/[0].Int
                )
            }
            when /R(<digit>+)/ {
                my $new-x = $x + $/[0].Int;
                take (
                    ($new-x, $y),
                    ($x...$new-x).map(-> $x-passed { ($x-passed, $y) }),
                    $path-length..^$path-length + $/[0].Int
                )
            }
            when /L(<digit>+)/ {
                my $new-x = $x - $/[0].Int;
                take (
                    ($new-x, $y),
                    ($x...$new-x).map(-> $x-passed { ($x-passed, $y) }),
                    $path-length..^$path-length + $/[0].Int
                )
            }
        }
    }.head;
    if @directions.elems > 1 {
        |(@spaces-passed Z @path-length).map(&tuple).Set ∪ traverse(@directions[1..*], @new-position, @path-length.max);
    } else {
        |(@spaces-passed Z @path-length).map(&tuple).Set;
    }
}

sub extract-coordinates($tuple) { tuple($tuple.key[0]) }
sub tuple-to-list($tuple) { ($tuple.key[0], $tuple.key[1]) }

sub MAIN($file, Bool :$p2 = False) {
    my ($wire1-path, $wire2-path) = $file.IO.lines.map(*.split(',')).map(-> @directions { traverse(@directions) });
    my $overlaps = ($wire1-path.map(&extract-coordinates) ∩ $wire2-path.map(&extract-coordinates));
    if $p2 {
        my @wire1-overlaps = $wire1-path
                               .grep(-> $entry { extract-coordinates($entry) ∈ $overlaps })
                               .map(&tuple-to-list)
                               .List
                               .sort({ $^a cmp $^b });
        my @wire2-overlaps = $wire2-path
                               .grep(-> $entry { extract-coordinates($entry) ∈ $overlaps })
                               .map(&tuple-to-list)
                               .List
                               .sort({ $^a cmp $^b });
        say |(@wire1-overlaps Z @wire2-overlaps)
              .map(-> @pair { @pair[0][1] + @pair[1][1] })
              .grep(* != 0)
              .min;
    } else {
        say $overlaps
            .map(-> $pair { $pair.key[0].abs + $pair.key[1].abs })
            .grep(* != 0)
            .min;
    }
}
