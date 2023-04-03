# 2016, Day 10.
# Input consists of instructions to run bots that possess and pass on physical chips. We simulate the behavior of these
# bots as they execute their rules.
#
# Part 1: Which robot will hold chips 17 and 61?
# Part 2: What is the product of the chips that end up in buckets 0, 1, and 2?

NAME = "Day 10: Balance Bots"

import re
from functools import reduce

valuesGoesToRegex = r'value (?P<value>\d+) goes to (?P<bot>bot \d+)'
botGivesRegex = r'(?P<bot>bot \d+) gives low to (?P<low>(bot|output) \d+) and high to (?P<high>(bot|output) \d+)'

def parseInstruction(line):
    def assign(m, bot, value):
        if bot not in m:
            m[bot] = [value]
        else:
            m[bot].append(value)

    def remove(m, bot, fn):
        x = fn(m[bot])
        m[bot].remove(x)
        return x

    def split(m, bot, dstLow, dstHigh):
        assign(m, dstLow, remove(m, bot, min))
        assign(m, dstHigh, remove(m, bot, max))

    # parseInstruction returns two lambdas, one for testing whether a rule can be executed,
    # and one to execute that rule. The first lambda is None if the instruction needs to be
    # executed only once.
    if m := re.search(valuesGoesToRegex, line):
        value, bot = int(m.group('value')), m.group('bot')
        return (None, lambda m: assign(m, bot, value))
    elif m := re.search(botGivesRegex, line):
        bot, low, high = m.group('bot'), m.group('low'), m.group('high')
        return (lambda m: bot in m and 2 <= len(m[bot]), lambda m: split(m, bot, low, high))

def parseInput(stream):
    return [parseInstruction(line.strip()) for line in stream.readlines()]

def prepareBots(instructions):
    m = dict()
    for ins in instructions:
        if ins[0] is None:
            ins[1](m)
    return m

def part1(instructions):
    m = prepareBots(instructions)
    while True:
        for (k, v) in m.items():
            if 17 in v and 61 in v:
                return k.split(' ')[1]

        for ins in instructions:
            if ins[0] is not None and ins[0](m):
                ins[1](m)

def part2(instructions):
    m = prepareBots(instructions)
    ks = ['output 0', 'output 1', 'output 2']
    while True:
        if all([k in m for k in ks]):
            return reduce(lambda x, y: x * y, [m[k][0] for k in ks])

        for ins in instructions:
            if ins[0] is not None and ins[0](m):
                ins[1](m)
