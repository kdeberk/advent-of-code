# 2016, Day 17.
# A maze of 4x4 rooms. Each pair of adjacent rooms is separated by a door that can
# be locked. Whether the door is locked is determined by the path that our hero took
# to reach the room with that door.
#
# Part 1: Find the shortest path to the bottom-right door.
#   We use a heap to quickly find a single path, and then only focus on the remaining
#   paths with a shorter length than the best found.
# Part 2: Find the length of the longest path that leads to the bottom-right door.
#   Simple BFS using a queue.

NAME = "Day 17: Two Steps Forward"

from heap import Heap
from md5 import md5

def parseInput(stream):
    return stream.read().strip()

UP    = lambda x, y: (x, y - 1)
DOWN  = lambda x, y: (x, y + 1)
LEFT  = lambda x, y: (x - 1, y)
RIGHT = lambda x, y: (x + 1, y)

VALID = set(["b", "c", "d", "e", "f"])

def openDoors(prefix, path):
    a, b, c, d = md5(prefix+path)[:4]
    doors = []
    if a in VALID:
        doors.append(('U', UP))
    if b in VALID:
        doors.append(('D', DOWN))
    if c in VALID:
        doors.append(('L', LEFT))
    if d in VALID:
        doors.append(('R', RIGHT))
    return doors

def part1(input):
    dist = lambda x, y: (3 - x) + (3 - y)
    shortest = None

    h = Heap()
    h.push(("", (0, 0)), dist(0, 0))
    while h.any():
        path, (x, y) = h.pop()
        if shortest is not None and len(shortest) <= len(path):
            continue

        for (n, door) in openDoors(input, path):
            nx, ny = door(x, y)
            if nx == 3 and ny == 3 and (shortest is None or len(path+n) < len(shortest)):
                shortest = path+n
            elif 0 <= nx and nx < 4 and 0 <= ny and ny < 4:
                h.push((path + n, (nx, ny)), dist(nx, ny))
    return shortest

def part2(input):
    longest = None

    q = [("", (0, 0))]
    while 0 < len(q):
        path, (x, y) = q.pop(0)
        for (n, door) in openDoors(input, path):
            nx, ny = door(x, y)
            if nx == 3 and ny == 3:
                if (longest is None or len(path+n) > longest):
                    longest = len(path+n)
            elif 0 <= nx and nx < 4 and 0 <= ny and ny < 4:
                q.append((path + n, (nx, ny)))
    return longest
