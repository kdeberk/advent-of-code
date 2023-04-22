# 2016, Day 24.

NAME = "Day 24: Air Duct Spelunking"

from geo import Point
from heap import MinHeap
from itertools import permutations, product

def findPoints(maze):
    return {int(cell): point
            for point, cell in maze.items()
            if cell.isnumeric()}

def shortestPath(maze, src, dst):  # Move to geo?
    h = MinHeap()
    h.push((0, src), 0)

    bestDists = {dst: float('inf')}
    while h.any():
        (nSteps, curPoint) = h.pop()

        if curPoint in bestDists and bestDists[curPoint] <= nSteps:
            continue # Already found same or better path.
        bestDists[curPoint] = nSteps

        if curPoint == dst:
            continue

        for neighbor in curPoint.neighbors():
            if neighbor not in maze:
                continue  # Not on the grid.
            if maze[neighbor] == '#':
                continue
            dist = nSteps + 1
            if neighbor in bestDists and bestDists[neighbor] <= dist:
                continue # Already found same or better path.

            h.push((dist, neighbor), dist + dst.dist(neighbor))
    return bestDists[dst]

def generateDistanceTable(maze):
    points = findPoints(maze)

    table = {}
    for (a, b) in product(points, points):
        if a == b:
            continue
        dist = shortestPath(maze, points[a], points[b])
        table[(a, b)] = dist
        table[(b, a)] = dist
    return table

def parseInput(stream):
    maze = {}
    for y, line in enumerate(stream.readlines()):
        for x, cell in enumerate(line.strip()):
            maze[Point(x, y)] = cell
    return generateDistanceTable(maze)

def part1(table):  # Combine with part2?
    shortest = None
    for perm in permutations(range(1, 8)):
        cur = 0
        dist = 0
        for point in perm:
            dist += table[(cur, point)]
            cur = point

        if shortest is None or dist < shortest:
            shortest = dist

    return shortest

def part2(table):
    shortest = None
    for perm in permutations(range(1, 8)):
        cur = 0
        dist = 0
        for point in perm:
            dist += table[(cur, point)]
            cur = point
        dist += table[(cur, 0)]

        if shortest is None or dist < shortest:
            shortest = dist

    return shortest
