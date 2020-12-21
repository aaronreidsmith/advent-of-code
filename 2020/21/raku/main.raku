#!/usr/bin/env raku

sub MAIN($file, Bool :$p2 = False) {
    my @rules = $file.IO.lines.map(-> $line {
        my ($ingredients-str, $allergens-str) = $line.split(' (contains ');
        my @ingredients = $ingredients-str.split(' ');
        my @allergens = $allergens-str.substr(0, *- 1).split(', ');
        { :@ingredients, :@allergens };
    });
    my $all-allergens = @rules.map(-> %rule { |%rule<allergens> }).Set;

    my %candidates = $all-allergens.keys.map(-> $allergen {
        $allergen => [∩] @rules.grep(*<allergens> ∋ $allergen).map(*<ingredients>);
    });

    my (%mapping, @mapped);
    while %mapping.elems < $all-allergens.elems {
        for %candidates.kv -> $allergen, $ingredients {
            if $ingredients.keys.elems == 1 {
                %mapping{$allergen} = $ingredients.keys.first;
                @mapped.push(%mapping{$allergen});
                %candidates{$allergen}:delete;
            }
        }
        for %candidates.keys -> $allergen {
            %candidates{$allergen} ∖= @mapped;
        }
    }

    if $p2 {
        say %mapping.sort.map(*.value).join(',');
    } else {
        say @rules.map((*<ingredients> ∖ @mapped).elems).sum;
    }
}
