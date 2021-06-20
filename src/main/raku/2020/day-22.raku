#!/usr/bin/env raku

sub calculate-score(@deck) {
    @deck.reverse.kv.map(-> $index, $value { ($index + 1) * $value }).sum;
}

sub play-game(@player1, @player2) {
    if @player1.elems == 0 {
        calculate-score(@player2);
    } elsif @player2.elems == 0 {
        calculate-score(@player1);
    } else {
        my $player1-card = @player1.head;
        my $player2-card = @player2.head;
        if $player1-card > $player2-card {
            play-game(
                (|@player1[1..*], $player1-card, $player2-card),
                @player2[1..*]
            );
        } else {
            play-game(
                @player1[1..*],
                (|@player2[1..*], $player2-card, $player1-card)
            );
        }
    }
}

sub MAIN($file) {
    my ($player1-str, $player2-str) = $file.IO.lines(:nl-in("\n\n"));
    my @player1 = $player1-str.lines[1..*].map(*.Int);
    my @player2 = $player2-str.lines[1..*].map(*.Int);
    say play-game(@player1, @player2);
}
