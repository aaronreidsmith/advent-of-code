#!/usr/bin/env python3.8

from collections import defaultdict
from itertools import product
import sys


def neighbors(coordinates):
    for diff in product([-1, 0, 1], repeat=len(coordinates)):
        yield tuple(coordinate + diff[i] for i, coordinate in enumerate(coordinates))


def conway_cubes(initial, dimensions):
    space = defaultdict(lambda: '.')
    padding = (0,) * (dimensions - 2)
    for x, line in enumerate(initial):
        for y, state in enumerate(line):
            cube = (x, y) + padding
            space[cube] = state

    for _ in range(6):
        active = defaultdict(int)

        for cube in space:
            if space[cube] == ".":
                continue
            for n in neighbors(cube):
                # We count active cubes with inactive neighbors so we can
                # activate/deactivate following as necessary in the next loop
                active[n] += 1 if n != cube else 0

        for n, count in active.items():
            if space[n] == "#":
                if count in (2, 3):
                    continue
                else:
                    space[n] = "."
            elif space[n] == ".":
                if count == 3:
                    space[n] = "#"

    return sum(state == "#" for state in space.values())


if __name__ == '__main__':
    initial = [line.strip() for line in open(sys.argv[1]).readlines()]
    print(f'Part 1: {conway_cubes(initial, dimensions=3)}')
    print(f'Part 2: {conway_cubes(initial, dimensions=4)}')
