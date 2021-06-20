#!/usr/bin/env perl6

# Int(Str) means it wants an Int but will allow a Str and coerce it to an Int
sub find-fuel(Int(Str) $module, $part-two) {
    my $required-fuel = $module div 3 - 2;
    # 8 div 3 - 2 = 0, so anything <= 8 requires no fuel
    if $part-two && $required-fuel > 8 {
        $required-fuel + find-fuel($required-fuel, $part-two)
    } else {
        $required-fuel;
    }
}

# https://adventofcode.com/2019/day/1
sub MAIN($file, Bool :$p2 = False) {
    say [+] $file.IO.lines.map(&find-fuel.assuming(*, $p2));
}
