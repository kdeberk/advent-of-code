# 2016, Day 19
# A certain number of elves are in a circle. They steal each others' presents until
# only one elf with presents is left.
#
# Part 1: A more kid-friendly version of the Josephus problem with k=2.
# Part 2: Elves take away presents from the elf opposite to them in the circle.

NAME = "Day 19: An Elephant Named Joseph"

from math import log

def parseInput(stream):
    return int(stream.read())

def part1(n):
    # Solution for Josephus problem with k=2 (immediate to the left) is cyclic bitshift,
    # last 1 bit is moved to the end.
    return 1 + ((n - (2**(n.bit_length()-1))) << 1)

def part2(n):
    # No idea why this formula works.
    p3 = 3**int(log(n-1, 3))
    if 2 * p3 < n:
        return 2*n-3*p3
    return n-p3
