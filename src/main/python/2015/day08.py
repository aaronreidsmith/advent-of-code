#!/usr/bin/env python

import json
import sys

if __name__ == '__main__':
    input = sys.argv[1]
    chars = 0
    memory = 0
    re_encoded = 0
    with open(input) as file:
        for line in file:
            trimmed = line.strip()
            chars += len(trimmed)
            memory += len(eval(trimmed))
            re_encoded += len(json.dumps(trimmed))

    print(f'Part 1: {chars - memory}')
    print(f'Part 2: {re_encoded - chars}')
