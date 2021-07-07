#!/usr/bin/env python3

# Not smart enough to figure this one out myself. Taken from here:
# https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4nsdd

import re
import sys

if __name__ == '__main__':
    input_file = sys.argv[1]
    input_contents = open(input_file).read()

    molecule = input_contents.split('\n')[-1][::-1]
    reps = {m[1][::-1]: m[0][::-1] for m in re.findall(r'(\w+) => (\w+)', input_contents)}

    count = 0
    while molecule != 'e':
        molecule = re.sub('|'.join(reps.keys()), lambda x: reps[x.group()], molecule, 1)
        count += 1

    print(count)
