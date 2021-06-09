#!/usr/bin/env raku

sub accumulate(@instructions is copy, $pointer = 0, $accumulator = 0, $part-two = False) {
    if $part-two && $pointer == @instructions.elems {
        ($accumulator, 'Terminal');
    } elsif @instructions[$pointer]<visited> {
        ($accumulator, 'Infinite');
    } else {
        @instructions[$pointer]<visited> = True;
        given @instructions[$pointer]<operation> {
            when 'acc' {
                accumulate(
                    @instructions,
                    $pointer + 1,
                    $accumulator + @instructions[$pointer]<value>,
                    $part-two
                );
            }
            when 'jmp' {
                accumulate(
                    @instructions,
                    $pointer + @instructions[$pointer]<value>,
                    $accumulator,
                    $part-two
                );
            }
            when 'nop' {
                accumulate(@instructions, $pointer + 1, $accumulator, $part-two);
            }
        }
    }
}


sub MAIN($file, Bool :$p2 = False) {
    my @cells = $file.IO.lines.map(-> $line {
        my ($operation, $value) = $line.split(' ');
        { :$operation, value => $value.Int, :!visited }
    });
    if $p2 {
        my @fixed-instructions = gather {
            for @cells.kv -> $index, %cell {
                given %cell<operation>.Str {
                    when /^[nop|jmp]$/ {
                        my @cells-copy = @cells.deepmap(-> $entry is copy { $entry });
                        my %cell-copy = %cell.deepmap(-> $entry is copy { $entry });
                        when 'nop' {
                            %cell-copy<operation> = 'jmp';
                            @cells-copy[$index] = %cell-copy;
                            take @cells-copy;
                        }
                        when 'jmp' {
                            %cell-copy<operation> = 'nop';
                            @cells-copy[$index] = %cell-copy;
                            take @cells-copy;
                        }
                    }
                }
            }
        };
        say @fixed-instructions
            .map(&accumulate.assuming(*, 0, 0, $p2))
            .grep(-> @pair { @pair[1] eq 'Terminal' })
            .head  # Gives us the first item in the above list
            .head; # Gives us the number in the pair returned from `accumulate`
    } else {
        say accumulate(@cells).head;
    }
}
