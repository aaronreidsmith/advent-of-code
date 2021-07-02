#!/usr/bin/env python3

import json
import re
import sys


def sum_numbers(string):
    return sum(int(num) for num in re.findall(r'-?\d+', string))


if __name__ == '__main__':
    input_file = sys.argv[1]
    raw_contents = ''.join(open(input_file).readlines())

    print(f'Part 1: {sum_numbers(raw_contents)}')

    filtered = str(json.loads(raw_contents, object_hook=lambda obj: {} if 'red' in obj.values() else obj))
    print(f'Part 2: {sum_numbers(filtered)}')
