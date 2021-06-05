#!/usr/bin/env python3

from collections import defaultdict
import sys


# https://www.reddit.com/r/adventofcode/comments/7kj35s/2017_day_18_solutions/drezydb?utm_source=share&utm_medium=web2x&context=3
class Program:
    def __init__(self, pid, other, instr):
        self.regs = defaultdict(int)
        self.regs['p'] = pid
        self.other = other
        self.instr = instr

        self.ip = 0
        self.buffer = []
        self.terminated = False
        self.blocked = False
        self.sent = 0

    def next(self):
        if self.terminated or self.ip < 0 or self.ip >= len(self.instr):
            self.terminated = True
            return
        ins = self.instr[self.ip].split()
        if ins[0] == 'snd':
            self.other.buffer.append(self.get(ins[1]))
            self.other.blocked = False
            self.sent += 1
        elif ins[0] == 'set':
            self.regs[ins[1]] = self.get(ins[2])
        elif ins[0] == 'add':
            self.regs[ins[1]] += self.get(ins[2])
        elif ins[0] == 'mul':
            self.regs[ins[1]] *= self.get(ins[2])
        elif ins[0] == 'mod':
            self.regs[ins[1]] %= self.get(ins[2])
        elif ins[0] == 'rcv':
            if len(self.buffer) > 0:
                self.regs[ins[1]] = self.buffer.pop(0)
            else:
                self.blocked = True
                return
        elif ins[0] == 'jgz':
            if self.get(ins[1]) > 0:
                self.ip += self.get(ins[2])
                return
        self.ip += 1

    def get(self, v):
        try:
            return int(v)
        except ValueError:
            return self.regs[v]


def solve_p2(instr):
    p0 = Program(0, None, instr)
    p1 = Program(1, p0, instr)
    p0.other = p1

    while not ((p0.terminated or p0.blocked) and (p1.terminated or p1.blocked)):
        p0.next()
        p1.next()

    return p1.sent


if __name__ == '__main__':
    instructions = open(sys.argv[1]).readlines()
    print(solve_p2(instructions))
