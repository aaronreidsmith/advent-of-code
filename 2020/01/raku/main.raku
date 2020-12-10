#!/usr/bin/env raku

sub MAIN($file, Bool :$p2 = False) {
    say $file.IO.lines
          .combinations($p2 ?? 3 !! 2)
          .grep(-> @combo { ([+] @combo) == 2020 })
          .map(-> @combo { [*] @combo })
          .head;
}
