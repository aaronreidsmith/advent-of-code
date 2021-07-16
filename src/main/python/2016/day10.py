#!/usr/bin/env python3

import collections
import re
import sys


bot = collections.defaultdict(list)
output = collections.defaultdict(list)

instructions = open(sys.argv[1]).readlines()

pipeline = {}
for line in instructions:
    if line.startswith('value'):
        value, bot_id = map(int, re.findall(r'\d+', line))
        bot[bot_id].append(value)
    if line.startswith('bot'):
        who, out1, out2 = map(int, re.findall(r'\d+', line))
        out_loc1, out_loc2 = re.findall(r' (bot|output)', line)
        pipeline[who] = (out_loc1, out1), (out_loc2, out2)


while bot:
    for bot_id, chips in dict(bot).items():
        if len(chips) == 2:
            low, high = sorted(bot.pop(bot_id))
            if low == 17 and high == 61:
                print(f'Part 1: {bot_id}')
            (out_loc1, out1), (out_loc2, out2) = pipeline[bot_id]
            eval(out_loc1)[out1].append(low)
            eval(out_loc2)[out2].append(high)


a, b, c = (output[loc][0] for loc in [0, 1, 2])
print(f'Part 2: {a * b * c}')
