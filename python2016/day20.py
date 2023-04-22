# 2016, Day 20
# A file contains a set of integer ranges. These ranges can overlap.
#
# Part 1: Find the lowest value not covered by any range.
# Part 2: Count the number of values not covered by any range.

NAME = "Day 20: Firewall Rules"

def parseInput(stream):
    blacklist = [list(int(n) for n in line.split("-"))
                 for line in stream.readlines()]
    return sorted(blacklist, key=lambda r: r[0])

def part1(blacklist):
    last = 0
    for (start, end) in blacklist:
        if last+1 < start:
            return last+1
        if last < end:
            last = end
    assert False

def part2(blacklist):
    last = 0
    count = 0
    for (start, end) in blacklist:
        if last+1 < start:
            count += (start - last) - 1
        if last < end:
            last = end
    return count
