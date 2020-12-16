#!/usr/bin/env raku

sub parse-input($file) {
    my ($train-text, $my-ticket, $other-tickets) = $file.IO.lines(:nl-in("\n\n"));

    # Turn train ranges into Junctions of Range objects
    my %train-info = $train-text.lines.map(-> $line {
        my ($range-name, $range-text) = $line.split(': ');
        my @ranges = $range-text.split(' or ').map(-> $range { $range.split('-').map(*.Int).minmax });
        $range-name => Junction.new(@ranges, type => 'any');
    }).Hash;

    # Map the tickets into lists of integers
    my @my-info = $my-ticket.lines[1].split(',').map(*.Int);
    my @other-info = $other-tickets.lines[1..*].map(-> $line { $line.split(',').map(*.Int) });

    (%train-info, @my-info, @other-info)
}

sub is-valid(@train-info, @ticket) {
    so all(@ticket) ∈ any(@train-info);
}

sub find-invalid-field(@train-info, @ticket) {
    for @ticket -> $field {
        if $field ∈ any(@train-info) {
            next;
        } else {
            return $field;
        }
    }
}

sub find-invalid-rate(@train-info, @tickets) {
    my @invalid = gather {
        for @tickets -> @ticket {
            if !is-valid(@train-info, @ticket) {
                take find-invalid-field(@train-info, @ticket);
            }
        }
    }
    @invalid.sum;
}

sub find-field-indices(%train-info, @fields) {
    # Given the size of our input ranges, some of our fields may fit into more than one category,
    # so we have to find all possible categories
    my %possible-fields;
    for @fields.kv -> $index, @field {
        for %train-info.kv -> $name, $range {
            if ?(all(@field) ∈ $range) {
                if %possible-fields{$name}:exists {
                    %possible-fields{$name} = (|%possible-fields{$name}, $index);
                } else {
                    %possible-fields{$name} = ($index,);
                }
            }
        }
    }

    my $number-of-fields = %possible-fields.elems;

    # We assume at least one of the above fields fits into exactly one category, so we look for
    # that item and then remove that index from the rest of the fields' categories and keep keep
    # looping until we have $number-of-fields defined
    my %final-fields;
    while %final-fields.elems != $number-of-fields {
        for %possible-fields.kv -> $name, @possible-indices {
            if @possible-indices.elems == 1 {
                my $index = @possible-indices.head;
                %final-fields{$name} = $index;
                %possible-fields{$name}:delete;
                for %possible-fields.kv -> $name-to-update, @indices-to-update {
                    %possible-fields{$name-to-update} = @indices-to-update.grep(* != $index);
                }
            }
        }
    }
    %final-fields;
}

sub MAIN($file, Bool :$p2 = False) {
    my (%train-info, @my-ticket, @other-tickets) := parse-input($file);
    my @all-tickets = (@my-ticket, |@other-tickets);
    if $p2 {
        my @valid-tickets = @other-tickets.grep(&is-valid.assuming(%train-info.values, *));
        my @fields = [Z] @valid-tickets;
        my %field-indices = find-field-indices(%train-info, @fields);
        my @departure-fields = gather {
            for %field-indices.kv -> $name, $index {
                if $name.starts-with('departure') {
                    take @my-ticket[$index];
                }
            }
        }
        say [*] @departure-fields;
    } else {
        say find-invalid-rate(%train-info.values, @all-tickets);
    }
}
