#!/usr/bin/env raku

sub MAIN($file, Bool :$p2 = False) {
    my @words = $file.IO.lines;
    if $p2 {
      for @words.combinations(2) -> ($first-word, $second-word) {
        my @differing-indices;
        my @first-chars = $first-word.comb;
        my @second-chars = $second-word.comb;
        for (@first-chars Z @second-chars).kv -> $index, ($first-char, $second-char) {
          next if $first-char eq $second-char || @differing-indices.elems > 1;
          @differing-indices.push($index);
        }
        last if @differing-indices == 1;

        LAST {
          my $index = @differing-indices.head;
          say @first-chars[^$index].join ~ @first-chars[$index^..*].join;
        }
      }
    } else {
        my @dupes = @words.grep(-> $word { $word.comb.Bag.values ∋ 2 });
        my @trips = @words.grep(-> $word { $word.comb.Bag.values ∋ 3 });
        say @dupes.elems * @trips.elems;
    }
}
