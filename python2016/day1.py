# 2016, Day 1.
# Input is a single line consisting of movement instructions separated by commas.
# Start facing north, each movement instruction dictates whether to turn left or
# right and then walking a certain distance on a manhattan grid.
#
# Part 1: Calculate final distance from starting point.
# Part 2: Determine first point on grid that we visit twice.

NAME = "Day 1: No Time for a Taxicab"

NORTH = lambda x, y, d : [x + d, y]
SOUTH = lambda x, y, d : [x - d, y]
WEST = lambda x, y, d : [x, y - d]
EAST = lambda x, y, d : [x, y + d]

LEFT  = {NORTH: WEST, WEST: SOUTH, SOUTH: EAST, EAST: NORTH}
RIGHT = {NORTH: EAST, EAST: SOUTH, SOUTH: WEST, WEST: NORTH}

def parseInput(stream):
    return [({'L': LEFT, 'R': RIGHT}[x[0]], int(x[1:]))
            for x in stream.read().strip().split(", ")]

def part1(moves):
    x, y, z = [0, 0, NORTH]
    for (c, d) in moves:
        z = c[z]
        x, y = z(x, y, d)
    return abs(x)+abs(y)

def part2(moves):
    seen = set()

    x, y, z = [0, 0, NORTH]
    for (c, d) in moves:
        z = c[z]

        # Visit each point along the way.
        for _ in range(d):
            if (x, y) in seen:
                return abs(x)+abs(y)
            seen.add((x, y))
            x, y = z(x, y, 1)
