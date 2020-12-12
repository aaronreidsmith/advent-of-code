#!/usr/bin/env raku

sub turn-ship($current-direction, $left-or-right, $degrees) {
    my %turns = (
        N => { L => 'W', R => 'E' },
        E => { L => 'N', R => 'S' },
        S => { L => 'E', R => 'W' },
        W => { L => 'S', R => 'N' }
    );

    if $degrees == 0 {
        $current-direction;
    } else {
        my $new-degrees = $degrees - 90;
        my $new-direction = %turns{$current-direction}{$left-or-right};
        turn-ship($new-direction, $left-or-right, $new-degrees);
    }
}

sub traverse-part1(
    @directions,
    $pointer = 0,
    $current-direction = 'E',
    @current-position = (0, 0)
) {
    if $pointer == @directions.elems {
        @current-position.map(*.abs).sum;
    } else {
        my ($i, $j) = @current-position;
        my $new-pointer = $pointer + 1;
        my $direction = @directions[$pointer].subst('F', $current-direction);
        given $direction {
            # Traversal directions
            when /N(<digit>+)/ {
                traverse-part1(
                    @directions,
                    $new-pointer,
                    $current-direction,
                    ($i - $/[0].Int, $j)
                );
            }
            when /E(<digit>+)/ {
                traverse-part1(
                    @directions,
                    $new-pointer,
                    $current-direction,
                    ($i, $j + $/[0].Int)
                );
            }
            when /S(<digit>+)/ {
                traverse-part1(
                    @directions,
                    $new-pointer,
                    $current-direction,
                    ($i + $/[0].Int, $j)
                );
            }
            when /W(<digit>+)/ {
                traverse-part1(
                    @directions,
                    $new-pointer,
                    $current-direction,
                    ($i, $j - $/[0].Int)
                );
            }
            # Turning directions
            when /L(<digit>+)/ {
                traverse-part1(
                    @directions,
                    $new-pointer,
                    turn-ship($current-direction, 'L', $/[0].Int),
                    ($i, $j)
                );
            }
            when /R(<digit>+)/ {
                traverse-part1(
                    @directions,
                    $new-pointer,
                    turn-ship($current-direction, 'R', $/[0].Int),
                    ($i, $j)
                );
            }
        }
    }
}

sub rotate-waypoint(@waypoint, $direction, $degrees) {
    if $degrees == 0 {
        @waypoint;
    } else {
        my ($i, $j) = @waypoint;
        my $new-degrees = $degrees - 90;
        given $direction {
            when 'L' { rotate-waypoint((-$j, $i), $direction, $new-degrees) }
            when 'R' { rotate-waypoint(($j, -$i), $direction, $new-degrees) }
        }
    }
}

sub traverse-part2(
    @directions,
    $pointer = 0,
    @current-position = (0, 0),
    @waypoint = (-1, 10)
) {
    if $pointer == @directions.elems {
        @current-position.map(*.abs).sum;
    } else {
        my ($waypoint-i, $waypoint-j) = @waypoint;
        my $new-pointer = $pointer + 1;
        given @directions[$pointer] {
            # Waypoint translation directions
            when /N(<digit>+)/ {
                traverse-part2(
                    @directions,
                    $new-pointer,
                    @current-position,
                    ($waypoint-i - $/[0].Int, $waypoint-j)
                );
            }
            when /E(<digit>+)/ {
                traverse-part2(
                    @directions,
                    $new-pointer,
                    @current-position,
                    ($waypoint-i, $waypoint-j + $/[0].Int)
                );
            }
            when /S(<digit>+)/ {
                traverse-part2(
                    @directions,
                    $new-pointer,
                    @current-position,
                    ($waypoint-i + $/[0].Int, $waypoint-j)
                );
            }
            when /W(<digit>+)/ {
                traverse-part2(
                    @directions,
                    $new-pointer,
                    @current-position,
                    ($waypoint-i, $waypoint-j - $/[0].Int)
                );
            }
            # Waypoint rotation directions
            when /L(<digit>+)/ {
                traverse-part2(
                    @directions,
                    $new-pointer,
                    @current-position,
                    rotate-waypoint(@waypoint, 'L', $/[0].Int)
                );
            }
            when /R(<digit>+)/ {
                traverse-part2(
                    @directions,
                    $new-pointer,
                    @current-position,
                    rotate-waypoint(@waypoint, 'R', $/[0].Int)
                );
            }
            # Ship-moving direction
            when /F(<digit>+)/ {
                my $number-of-moves = $/[0].Int;
                my ($i, $j) = @current-position;
                my $new-i = $i + ($number-of-moves * $waypoint-i);
                my $new-j = $j + ($number-of-moves * $waypoint-j);
                traverse-part2(
                    @directions,
                    $new-pointer,
                    ($new-i, $new-j),
                    @waypoint
                );
            }
        }
    }
}

sub MAIN($file, Bool :$p2 = False) {
    my @directions = $file.IO.lines;
    say $p2 ?? traverse-part2(@directions) !! traverse-part1(@directions);
}
