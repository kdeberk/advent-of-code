# 2016, Day 22.
# We need to move data within a series of connected disks.
#
# We have 33x30 = 990 disks, the data at disk node-x32-y0 needs to move to disk node-x0-y0.
# There is a single empty disk, and no non-empty disk can hold its own plus data from another disk.
#
# Part 1: Count all nodes that can offload their data to any other node.
# Part 2: Count the number of steps needed to move the data from one disk to another.
#   I first solved it manually, then wrote the code.

NAME = "Day 22: Grid Computing"

import re
from geo import Point
from heap import MinHeap

fileRegex = r'/dev/grid/node-x(?P<x>\d+)-y(?P<y>\d+)\s+(?P<size>\d+)T\s+(?P<used>\d+)T\s+(?P<avail>\d+)T'

class Node:
    def __init__(self, x, y, used, avail, total):
        self.x, self.y, self.used, self.avail, self.total = x, y, used, avail, total
    def __str__(self):
        return f'{self.used}, {self.avail}'

def parseInput(stream):
    grid = {}
    for line in stream.readlines()[2:]:
        m = re.search(fileRegex, line).groupdict()
        x, y, used, avail = int(m['x']), int(m['y']), int(m['used']), int(m['avail'])
        grid[Point(x, y)] =  Node(x, y, used, avail, used+avail)
    return grid

def part1(grid):
    byAvail = sorted(grid.values(), key=lambda node: node.avail)

    count = 0
    availIdx = 0
    for used in sorted(grid.values(), key=lambda node: node.used):
        while availIdx < len(grid) and byAvail[availIdx].avail < used.used:
            availIdx += 1
        if availIdx == len(byAvail):
            break
        if 0 < used.used and used != byAvail[availIdx]:
            count += len(grid) - availIdx
    return count

# Implementation of A* where we don't care about the route per se, but only about
# the minimum number of steps.
def moveZero(grid, src, dst):
    h = MinHeap()

    bestDists = {dst: float('inf')}
    h.push((0, src), 0)
    while h.any():
        (nSteps, curPoint) = h.pop()

        if curPoint in bestDists and bestDists[curPoint] <= nSteps:
            continue # Already found same or better path.
        bestDists[curPoint] = nSteps

        if curPoint == dst:
            continue

        for neighbor in curPoint.neighbors():
            if neighbor not in grid:
                continue  # Not on the grid.
            if grid[curPoint].total <= grid[neighbor].used:
                continue
            dist = nSteps + 1
            if neighbor in bestDists and bestDists[neighbor] <= dist:
                continue # Already found same or better path.

            h.push((dist, neighbor), dist + dst.dist(neighbor))

    return bestDists[dst]

def part2(grid):
    maxX = max(p.x for p in grid)

    dataSrc = Point(maxX, 0)
    dataDst = Point(0, 0)

    zeroSrc = next(p for p, n in grid.items() if n.used == 0)
    zeroDst = Point(dataSrc.x - 1, 1) # SW of the data source.

    # Add three values:
    # - number of steps needed to empty the node SW of the data source,
    # - number of steps needed to move the desired data one node to the left,
    #
    #     . G   =>  _ G   =>  G _
    #     _ .       . .       . .
    #
    #   Number of steps: 2.
    #
    # - number of steps to move the desired data all the way to (0, 0)
    #   Every time we move the value one step to the left, we first need to move data from 5 other nodes.
    #
    #   . G _  =>  . G .  =>  . G .  =>  . G .  =>  _ G .  =>  G _ .
    #   . . .      . . _      . _ .      _ . .      . . .      . . .
    #
    #   Since each sequence of 5 steps leaves the grid in the same state as before, except with
    #   the window moved a step to the left, we simply calculate 5 * (dist(dst, src) - 1) as that is the
    #   number of steps that the desired data itself has to move.
    #
    return moveZero(grid, zeroSrc, zeroDst) + \
        2 + \
        5 * (dataSrc.dist(dataDst) - 1)
