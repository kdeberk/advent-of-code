# 2016, Day 13.
# Input is used to generate a maze. This maze is navigated with BFS and a simple FIFO queue.
#
# Part 1: Length of shortest path to (31, 39)
# Part 2: Number of open cells you can reach within 50 steps of starting position.

NAME = "Day 13: A Maze of Twisty Little Cubicles"

from geo import Point

WALL = '#'
OPEN = '.'

class Grid:
    def __init__(self, secret):
        self.secret = secret
        self.cells = {}

    def get(self, p: Point):
        if p not in self.cells:
            self.cells[p] = self.cell(p.x, p.y)
        return self.cells[p]

    def cell(self, x, y):
        v = x*x + 3*x + 2*x*y + y + y*y + self.secret
        c = 0
        while 0 < v:
            if v % 2 == 1:
                c += 1
            v >>= 1
        if c % 2 == 1:
            return WALL
        return OPEN

# walkGrid traverses the open cells of the grid using BFS. That guarantees that for every new cell it
# reaches, it came there by using the shortest path.
def walkGrid(grid, fn):
    seen = {Point(0, 0): (0, OPEN)}

    q = [(0, Point(0, 0))]
    while 0 < len(q):
        (steps, p) = q.pop(0)

        if fn(steps, p):
            return steps, p, seen

        for n in p.neighbors():
            if n.x < 0 or n.y < 0:
                continue

            if n in seen:
                continue

            t = grid.get(n)
            seen[n] = (steps + 1, t)

            if (t == WALL):
                continue

            q.append((steps + 1, n))
    assert False

def part1():
    grid = Grid(1364)
    steps, _, _ = walkGrid(grid, lambda _, p: p == Point(31, 39))
    return steps

def part2():
    grid = Grid(1364)
    _, _, seen = walkGrid(grid, lambda steps, _: steps == 50)
    # The seen dictionary maps Point(x, y) positions to fewest number of steps to reach and OPEN/WALL. We filter and
    # count using a list comprehension.
    return len([v for v in seen.values() if v[0] < 50 and v[1] == OPEN])
