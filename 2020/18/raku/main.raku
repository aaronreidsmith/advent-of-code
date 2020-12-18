#!/usr/bin/env raku

use MONKEY-SEE-NO-EVAL;

sub infix:<p>(Int:D $a, Int:D $b) returns Int:D is equiv(&infix:<*>) { $a + $b }
sub infix:<m>(Int:D $a, Int:D $b) returns Int:D is looser(&infix:<p>) { $a * $b }

sub MAIN($file, Bool :$p2 = False) {
   my @expressions = $file.IO.lines.map(*.trans('+' => 'p'));
   if $p2 {
        say @expressions.map(*.trans('*' => 'm')).map(-> $expr { EVAL $expr }).sum;
   } else {
       say @expressions.map(-> $expr { EVAL $expr }).sum;
   }
}
