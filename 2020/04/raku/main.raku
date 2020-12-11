#!/usr/bin/env raku

sub is-valid(%credentials, $check-values) {
    my $passport-keys = set <byr iyr eyr hgt hcl ecl pid cid>;
    my $north-pole-credentials = $passport-keys ⊖ 'cid';

    my $keys = set %credentials.keys;
    my $valid-keys = $keys ~~ $passport-keys || $keys ~~ $north-pole-credentials;

    if $valid-keys && $check-values {
        my ($byr, $iyr, $eyr, $hgt, $hcl, $ecl, $pid) = %credentials<byr iyr eyr hgt hcl ecl pid>.map(*.Str);

        my $valid-byr = so $byr ~~ /^<digit> ** 4$/ && $byr.Int ∈ set (1920..2002);
        my $valid-iyr = so $iyr ~~ /^<digit> ** 4$/ && $iyr.Int ∈ set (2010..2020);
        my $valid-eyr = so $eyr ~~ /^<digit> ** 4$/ && $eyr.Int ∈ set (2020..2030);
        my $valid-hgt = gather {
            given $hgt {
                when /^(<digit>+)'in'$/ { take $/[0].Int ∈ set (59..76) }
                when /^(<digit>+)'cm'$/ { take $/[0].Int ∈ set (150..193) }
                default                 { take False }
            }
        }.head;
        my $valid-hcl = so $hcl ~~ /^'#'<xdigit> ** 6$/;
        my $valid-ecl = $ecl ∈ set <amb blu brn gry grn hzl oth>;
        my $valid-pid = so $pid ~~ /^<digit> ** 9$/;
        $valid-byr && $valid-iyr && $valid-eyr && $valid-hgt && $valid-hcl && $valid-ecl && $valid-pid;
    } else {
        $valid-keys;
    }
}

sub MAIN($file, Bool :$p2 = False) {
    say $file.IO
          .slurp
          .split(/\n\n/)
          .map(-> $entry {
              $entry
                .split(/<space>/)
                .map(*.split(':'))
                .map(-> ($key, $value) { $key.trim => $value.trim })
                .Hash
          })
          .map(&is-valid.assuming(*, $p2))
          .grep(* == True)
          .elems;
}
