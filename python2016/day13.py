# 2016, Day 13.
# Input is used to generate a maze. This maze is navigated with BFS and a simple FIFO queue.
#
# Part 1: Length of shortest path to (31, 39)
# Part 2: Number of open cells you can reach within 50 steps of starting position.

WALL = '#'
OPEN = '.'

class Grid:
    def __init__(self, secret):
        self.secret = secret
        self.cells = {}

    def get(self, x, y):
        if (x, y) not in self.cells:
            self.cells[(x, y)] = self.cell(x, y)
        return self.cells[(x, y)]

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

# walkGrid traverses the entire grid using BFS. That guarantees that for every new cell it
# reaches, it came there by using the shortest path.
def walkGrid(grid, fn):
    seen = {(0, 0): (0, OPEN)}

    q = [(0, (0, 0))]
    while 0 < len(q):
        (steps, (x, y)) = q.pop(0)

        if fn(steps, (x, y)):
            return steps, (x, y), seen

        for (dx, dy) in [(1, 0), (-1, 0), (0, 1), (0, -1)]:
            if x + dx < 0 or y + dy < 0:
                continue

            if (x + dx, y + dy) in seen:
                continue

            t = grid.get(x + dx, y + dy)
            seen[(x + dx, y + dy)] = (steps + 1, t)

            if (t == WALL):
                continue

            q.append((steps + 1, (x + dx, y + dy)))

def part1(grid):
    steps, _, _ = walkGrid(grid, lambda _, xy: xy == (31, 39))
    return steps

def part2(grid):
    _, _, seen = walkGrid(grid, lambda steps, _: steps == 50)
    # the seen dictionary maps (x, y) positions to fewest number of steps to reach and OPEN/WALL. We filter and
    # count using a list comprehension.
    return len([v for v in seen.values() if v[0] < 50 and v[1] == OPEN])

if __name__ == "__main__":
    grid = Grid(1364)
    print("Part 1:", part1(grid))
    print("Part 2:", part2(grid))
