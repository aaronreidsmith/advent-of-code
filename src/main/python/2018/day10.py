#!/usr/bin/env python3

from collections import deque, defaultdict

import re
import sys


def play_game(max_players, last_marble):
    scores = defaultdict(int)
    circle = deque([0])

    for marble in range(1, last_marble + 1):
        if marble % 23 == 0:
            circle.rotate(7)
            scores[marble % max_players] += marble + circle.pop()
            circle.rotate(-1)
        else:
            circle.rotate(-1)
            circle.append(marble)

    return max(scores.values()) if scores else 0


if __name__ == '__main__':
    file = sys.argv[1]
    contents = open(file).read()
    match = re.search(r'^(\d+) players; last marble is worth (\d+) points$', contents)
    max_players = int(match.group(1))
    last_marble = int(match.group(2))
    if len(sys.argv) > 2:
        print(play_game(max_players, last_marble * 100))
    else:
        print(play_game(max_players, last_marble))
