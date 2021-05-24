#!/usr/bin/env raku

sub parse(Str $file) {
  gather for $file.IO.lines -> $line {
    if $line ~~ m:s/^'['(<digit>**4)'-'(<digit>**2)'-'(<digit>**2) (<digit>**2)':'(<digit>**2)']' (.*)$/ {
      take {
        timestamp => DateTime.new(
          year   => $/[0].Int,
          month  => $/[1].Int,
          day    => $/[2].Int,
          hour   => $/[3].Int,
          minute => $/[4].Int
        ),
        memo => $/[5]
      };
    }
  }.sort(*<timestamp>);
}

sub MAIN(Str $file, Bool :$p2 = False) {
  my ($current-guard, $current-start, $current-end);
  my %guard-shifts;
  for parse($file) -> %entry {
    my $memo = %entry<memo>;
    given $memo {
      when m:s/Guard '#'(<digit>+) begins shift/ {
        $current-guard = $/[0].Int;
        $current-start = Nil;
        $current-end   = Nil;
      }
      when 'falls asleep' {
        $current-start = %entry<timestamp>;
        $current-end   = Nil;
      }
      when 'wakes up' {
        $current-end     = %entry<timestamp>;
        my $start-minute = $current-start.minute;
        my $end-minute   = $current-end.minute;
        if %guard-shifts{$current-guard}:!exists {
          %guard-shifts{$current-guard} = {
             0 => 0,  1 => 0,  2 => 0,  3 => 0,  4 => 0,
             5 => 0,  6 => 0,  7 => 0,  8 => 0,  9 => 0,
            10 => 0, 11 => 0, 12 => 0, 13 => 0, 14 => 0,
            15 => 0, 16 => 0, 17 => 0, 18 => 0, 19 => 0,
            20 => 0, 21 => 0, 22 => 0, 23 => 0, 24 => 0,
            25 => 0, 26 => 0, 27 => 0, 28 => 0, 29 => 0,
            30 => 0, 31 => 0, 32 => 0, 33 => 0, 34 => 0,
            35 => 0, 36 => 0, 37 => 0, 38 => 0, 39 => 0,
            40 => 0, 41 => 0, 42 => 0, 43 => 0, 44 => 0,
            45 => 0, 46 => 0, 47 => 0, 48 => 0, 49 => 0,
            50 => 0, 51 => 0, 52 => 0, 53 => 0, 54 => 0,
            55 => 0, 56 => 0, 57 => 0, 58 => 0, 59 => 0,
            -1 => 0 # Key for "total minutes asleep"
          };
        }
        for ($start-minute..^$end-minute) -> $minute {
          %guard-shifts{$current-guard}{$minute} += 1;
          %guard-shifts{$current-guard}{-1} += 1;
        }
        $current-start = Nil;
        $current-end   = Nil;
      }
    }
  }
  if $p2 {
    my %most-asleep-guard := {
      id                       => -1,
      minute-most-asleep-key   => -1,
      minute-most-asleep-value => -1
    };
    for %guard-shifts.kv -> $id, %minutes-asleep {
      %minutes-asleep{-1}:delete;
      my $minute-most-asleep = %minutes-asleep.max(*.value);
      if $minute-most-asleep.value > %most-asleep-guard<minute-most-asleep-value> {
        %most-asleep-guard<id>                       = $id;
        %most-asleep-guard<minute-most-asleep-key>   = $minute-most-asleep.key;
        %most-asleep-guard<minute-most-asleep-value> = $minute-most-asleep.value;
      }
    }
    say %most-asleep-guard<id> * %most-asleep-guard<minute-most-asleep-key>;
  } else {
    # .max() doesn't seem to work properly, so this is our workaround
    my %most-asleep-guard := {
      id             => -1,
      minutes-asleep => 0
    };
    for %guard-shifts.kv -> $id, %minutes-asleep {
      if %minutes-asleep{-1} > %most-asleep-guard<minutes-asleep> {
        %most-asleep-guard<id>             = $id;
        %most-asleep-guard<minutes-asleep> = %minutes-asleep{-1};
      }
    }
    my $most-asleep-guard  = %most-asleep-guard<id>;

    # Have to delete the `-1` key or we will get bad results
    my %all-minutes-asleep = %guard-shifts{$most-asleep-guard};
    %all-minutes-asleep{-1}:delete;
    my $most-asleep-minute = %all-minutes-asleep.max(*.value).key;

    say $most-asleep-guard * $most-asleep-minute;
  }
}
