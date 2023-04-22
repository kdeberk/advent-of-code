# 2016, Day 21.
# Hash a given string using only positional operations. The characters in the
# string are not modified themselves, but only change positions in the string.
# The trick is to implement the operations and their reversals correctly.
#
# Part 1: Hash the string 'abcdefgh' according to the operations in the input file.
# Part 2: Determine original input that produced 'fbgdceah' after applying the
#   instructions in the input file.

NAME = "Day 21: Scrambled Letters and Hash"

import re
from functools import reduce

# Grouping the parsing and execution logic in a single class per operation
# allows us to place related logic nearby each other.

class SwapPosition:
    REGEX = r'swap position (\d+) with position (\d+)'
    def __init__(self, m):
        self.a, self.b = int(m[0]), int(m[1])
    def exec(self, state):
        state[self.a], state[self.b] = state[self.b], state[self.a]
        return state
    rev_exec = exec

class SwapLetter:
    REGEX = r'swap letter ([a-z]) with letter ([a-z])'
    def __init__(self, m):
        self.a, self.b = m[0], m[1]
    def exec(self, state):
        idx, jdx = state.index(self.a), state.index(self.b)
        state[idx], state[jdx] = state[jdx], state[idx]
        return state
    rev_exec = exec

class RotateOnPosition:
    REGEX = r'rotate based on position of letter ([a-z])'
    def __init__(self, m):
        self.l = m[0]
    def exec(self, state):
        idx = state.index(self.l)
        rot = (idx + 1 + (1 if 4 <= idx else 0)) % len(state)
        return state[-rot:] + state[:-rot]
    def rev_exec(self, state):
        idx = state.index(self.l)
        for jdx in range(0, len(state)):
            rot = (jdx + 1 + (1 if 4 <= jdx else 0)) % len(state)
            if idx == (jdx+rot)%len(state):
                return state[idx-jdx:] + state[:idx-jdx]
        assert False

class Rotate:
    REGEX = r'rotate (left|right) (\d+) steps?'
    def __init__(self, m):
        self.dir, self.nsteps = m[0], int(m[1])
    def exec(self, state):
        rot = -self.nsteps if 'left' == self.dir else self.nsteps
        return state[-rot:] + state[:-rot]
    def rev_exec(self, state):
        rot = self.nsteps if 'left' == self.dir else -self.nsteps
        return state[-rot:] + state[:-rot]

class ReversePositions:
    REGEX = r'reverse positions (\d) through (\d)'
    def __init__(self, m):
        self.start, self.end = int(m[0]), int(m[1])
    def exec(self, state):
        return state[:self.start] + list(reversed(state[self.start:self.end+1])) + state[self.end+1:]
    rev_exec = exec

class Move:
    REGEX = r'move position (\d) to position (\d)'
    def __init__(self, m):
        self.src, self.dst = int(m[0]), int(m[1])
    def exec(self, state):
        return self.move(state, self.src, self.dst)
    def rev_exec(self, state):
        return self.move(state, self.dst, self.src)
    def move(self, state, src, dst):
        if src < dst:
            return state[:src] + state[src+1:dst+1] + [state[src]] + state[dst+1:]
        return state[:dst] + [state[src]] + state[dst:src] + state[src+1:]

def parseInput(stream):
    ins = []
    for line in stream.readlines():
        for k in [SwapPosition, SwapLetter, RotateOnPosition, Rotate, ReversePositions, Move]:
            if m := re.search(k.REGEX, line):
                ins.append(k(m.groups()))
                continue
    return ins

def part1(ins):
    return ''.join(reduce(lambda state, ins: ins.exec(state), ins, list('abcdefgh')))

def part2(ins):
    return ''.join(reduce(lambda state, ins: ins.rev_exec(state), reversed(ins), list('fbgdceah')))
