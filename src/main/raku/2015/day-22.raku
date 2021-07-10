#!/usr/bin/env perl6

# Adapted from https://www.reddit.com/r/adventofcode/comments/3xspyl/day_22_solutions/cy90jz3
sub MAIN(Str $file) {
  my ($boss-hp, $boss-attack) = $file.IO.slurp.split("\n").map(*.split(": ").tail.Int);
  my %base-stats := {
    :$boss-hp,
    player-hp   => 50,
    armor       => 0,
    player-mana => 500,
    mana-spent  => 0,
    shield      => 0,
    poison      => 0,
    recharge    => 0
  };
  my %stats = %base-stats.clone;
  my %spells := {
    'Magic Missle' => 53,
    'Drain'        => 73,
    'Shield'       => 113,
    'Poison'       => 173,
    'Recharge'     => 229
  };

  sub fight(Bool $part2) {
    %stats = %base-stats;

    my $turn = 'player';

    loop {
      if %stats<shield> > 0 {
        %stats<armor> = 7;
        %stats<shield> -= 1;
      } else {
        %stats<armor> = 0;
      }

      if %stats<poison> > 0 {
        %stats<boss-hp> -= 3;
        if %stats<boss-hp> <= 0 {
          return ('player', %stats<mana-spent>);
        }
        %stats<poison> -= 1;
      }

      if %stats<recharge> > 0 {
        %stats<player-mana> += 101;
        %stats<recharge> -= 1;
      }

      if $turn eq 'player' {

        if $part2 {
          %stats<player-hp> -= 1;
          if %stats<player-hp> <= 0 {
            return ('boss', %stats<mana-spent>);
          }
        }

        my $spell;
        for %spells.pick(*) -> $pair {
          my $spell-name = $pair.key;
          my $cost = $pair.value;
          if %stats<player-mana> >= $cost {
            next if $spell-name eq 'Shield' && %stats<shield> > 0;
            next if $spell-name eq 'Poison' && %stats<poison> > 0;
            next if $spell-name eq 'Recharge' && %stats<recharge> > 0;
            $spell = $spell-name;
            last;
          }
        }

        if !$spell.defined {
          return ('boss', %stats<mana-spent>);
        }

        %stats<player-mana> -= %spells{$spell};
        %stats<player-mana> = 0 if %stats<player-mana> < 0;
        %stats<mana-spent> += %spells{$spell};

        given $spell {
          when 'Magic Missle' {
            %stats<boss-hp> -= 4;
            if %stats<boss-hp> <= 0 {
              return ('player', %stats<mana-spent>);
            }
          }
          when 'Drain' {
            %stats<boss-hp> -= 2;
            if %stats<boss-hp> <= 0 {
              return ('player', %stats<mana-spent>);
            }
            %stats<player-hp> += 2;
          }
          when 'Shield' {
            %stats<armor> = 7;
            %stats<shield> = 6;
          }
          when 'Poison' {
            %stats<poison> = 6;
          }
          when 'Recharge' {
            %stats<recharge> = 5;
          }
        }
        $turn = 'boss'
      } else {
        my $dmg = max(1, $boss-attack - %stats<armor>);
        %stats<player-hp> -= $dmg;
        if %stats<player-hp> <= 0 {
          return ('boss', %stats<mana-spent>);
        }
        $turn = 'player'
      }
    }
  }

  my @part1-results;
  for 1..10_000 {  # 10,000 consistently gets the right answer
    my ($winner, $mana-spent) = fight(False);
    if $winner eq 'player' {
      @part1-results.push($mana-spent);
    }
  }
  say "Part 1: {@part1-results.min}";

  my @part2-results;
  for 1..100_000 {  # Part 2 needed 100,000 to get the right answer consistently
    my ($winner, $mana-spent) = fight(True);
    if $winner eq 'player' {
      @part2-results.push($mana-spent);
    }
  }
  say "Part 2: {@part2-results.min}";
}
