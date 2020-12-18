#!/usr/bin/env raku

use MONKEY-SEE-NO-EVAL;

sub infix:<plus>(Int:D $a, Int:D $b) returns Int:D is equiv(&infix:<*>) { $a + $b }
sub infix:<mult>(Int:D $a, Int:D $b) returns Int:D is looser(&infix:<plus>) { $a * $b }

sub MAIN($file, Bool :$p2 = False) {
   my @expressions = $file.IO.lines.map(*.trans(['+'] => ['plus']));
   if $p2 {
        say @expressions.map(*.trans(['*'] => ['mult'])).map(-> $expr { EVAL $expr }).sum;
   } else {
       say @expressions.map(-> $expr { EVAL $expr }).sum;
   }
}
