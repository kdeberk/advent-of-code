# 2016, Day 24.
# Determine the shortest path that a robot can move through an airduct maze.
#
# Part 1: Calculate the length of the shortest route that starts with 0 and
#   visits all points of interest
# Part 2: Calculate the length of the shortest Hamiltonian cycle.

NAME = "Day 24: Air Duct Spelunking"

from geo import Point
from itertools import permutations

def findPoints(maze):
    return {int(cell): point
            for point, cell in maze.items()
            if cell.isnumeric()}

def generateDistanceTable(maze):
    points = findPoints(maze)
    table = {}

    for interest, point in points.items():
        bestDists = {}
        queue = [(0, point)]

        while 0 < len(queue):
            (nSteps, curPoint) = queue.pop(0)

            if curPoint in bestDists and bestDists[curPoint] <= nSteps:
                continue # Better path was found
            bestDists[curPoint] = nSteps

            for neighbor in curPoint.neighbors():
                if neighbor not in maze:
                    continue # Either wall or outside of bounds.
                if neighbor in bestDists and bestDists[neighbor] < nSteps+1:
                    continue # Better path was found.
                queue.append((nSteps+1, neighbor))

        for i in points:
            table[(interest, i)] = bestDists[points[i]]
            table[(i, interest)] = bestDists[points[i]]
    return table

def parseInput(stream):
    maze = {}
    for y, line in enumerate(stream.readlines()):
        for x, cell in enumerate(line.strip()):
            if cell == '#':
                continue # Skip walls
            maze[Point(x, y)] = cell
    return generateDistanceTable(maze)

def pathLength(path, table):
    l = 0
    cur = 0
    for p in path:
        l += table[(cur, p)]
        cur = p
    return l

def part1(table):
    return min(pathLength([0] + list(perm), table)
               for perm in permutations(range(1, 8)))

def part2(table):
    return min(pathLength([0] + list(perm) + [0], table)
               for perm in permutations(range(1, 8)))
