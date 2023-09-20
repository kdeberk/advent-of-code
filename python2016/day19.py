# 2016, Day 19
# A certain number of elves are in a circle. They steal each others' presents until
# only one elf with presents is left.
#
# Part 1: A more kid-friendly version of the Josephus problem with k=2.
#   There is a fast solution using the length of the bit represention,
#   but let's instead use a simple solution that we can understand:
#   - We start from a circle of 2 elves and we know the outcome: the 1st elf wins.
#   - Then we keep increasing the circle size by one, and at every step we use the fact that
#     as soon as elf 1 has taken his presents from elf 2 and elf 3 gets his first turn,
#     that the situation is the same as with the previous circle in which we already calculated
#     the surviving elf relative to the first elf that gets to take the presents.
#   - when the circle size is a power of 2, then the first elf always wins.
#     So we start from the largest power of 2 that is smaller than the input.
# Part 2: Elves take away presents from the elf opposite to them in the circle.
#   Very similar to part 1, but we need to 'jump' over the opposite elf.

NAME = "Day 19: An Elephant Named Joseph"

from math import floor, log

def parseInput(stream):
    return int(stream.read())

def part1(input):
    circleSize, winner = 2**floor(log(input, 2)), 0  # 1st elf wins in game with 2**x elves.

    while circleSize < input:
        circleSize += 1
        # We 'recurse' from the perspective of the 3rd elf.
        winner = (2 + winner) % circleSize
    return winner+1

def part2(input):
    circleSize, winner = 3, 2  # 3rd elf wins in game with 3 elves.

    while circleSize < input:
        circleSize += 1
        # We 'recurse' from the perspective of the 2nd elf.
        winner = (1 + winner) % circleSize
        # If the 'winning' elf is in the 2nd half of the circle, then we need to increment w,
        # since we need to account for the elf whose presents were taken.
        if winner >= circleSize//2:
            winner = (winner + 1) % circleSize
    return winner+1
