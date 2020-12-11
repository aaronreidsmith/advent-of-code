#!/usr/bin/env raku

enum SeatState <Occupied Empty Floor>;

sub find-equilibrium(@state, Int $min-occupied, Bool $only-immediate) {
    my ($max-row, $max-col) = @state.elems, @state[0].elems;
    my @directions = (
        (-1, 0), # N
        (-1, 1), # NE
        (0, 1),  # E
        (1, 1),  # SE
        (1, 0),  # S
        (1, -1), # SW
        (0, -1), # W
        (-1, -1) # NW
    );

    sub occupied-neighbors(@seat-map, $row, $col) {
        sub neighbor(@direction) {
            my ($new-row, $new-col) = $row, $col;
            my $valid-seats = Set[SeatState].new(Occupied, Empty);
            loop {
                $new-row += @direction[0];
                $new-col += @direction[1];
                if ($new-row < 0 || $new-row >= $max-row || $new-col < 0 || $new-col >= $max-col) {
                    return 'Out of Bounds';
                } else {
                    my $seat = @seat-map[$new-row][$new-col];
                    if $seat ∈ $valid-seats || $only-immediate {
                        return $seat;
                    }
                }
            }
        }
        @directions.map(&neighbor).grep(* eq Occupied).elems;
    }

    my @old-state;
    while @old-state ne @state {
        @old-state = @state;
        @state = ();
        for ^$max-row -> $row {
            my @new-row;
            for ^$max-col -> $col {
                my $current-seat = @old-state[$row][$col];
                my $occupied-neighbors = occupied-neighbors(@old-state, $row, $col);
                if $current-seat eq Empty && $occupied-neighbors == 0 {
                    @new-row.push(Occupied);
                } elsif $current-seat eq Occupied && $occupied-neighbors >= $min-occupied {
                    @new-row.push(Empty);
                } else {
                    @new-row.push($current-seat);
                }
            }
            @state.push(@new-row);
        }
    }
    @old-state[*;*].grep(* eq Occupied).elems;
}


sub MAIN($file, Bool :$p2 = False) {
    my @seats = $file.IO.lines.map(-> $line {
        $line.comb.map(-> $char {
            given $char {
                when '#' { Occupied }
                when 'L' { Empty }
                when '.' { Floor }
            }
        })
    });
    my $min-occupied = $p2 ?? 5 !! 4;
    my $only-immediate = !$p2;
    say find-equilibrium(@seats, $min-occupied, $only-immediate);
}
