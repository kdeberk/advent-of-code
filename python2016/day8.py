# 2016, Day 8.
# Input contains render instructions for a display.
#
# Part 1: Excecute the render instructions and count the number of display cells that are turned on.
# Part 2: Render the display cells in a 50x6 grid and observe the password.

NAME = "Day 8: Two-Factor Authentication"

import re

WIDTH = 50
HEIGHT = 6

rectRegex = r'^rect (?P<width>\d+)x(?P<height>\d+)$'
rotateRowRegex = r'^rotate row y=(?P<row>\d+) by (?P<by>\d+)'
rotateColRegex = r'^rotate column x=(?P<col>\d+) by (?P<by>\d+)'

def rightRotate(l, n):
    return l[-n:] + l[:-n]

def render(lines):
    grid = [[' ' for _ in range(0, WIDTH)] for _ in range(0, HEIGHT)]

    for line in lines:
        if m := re.match(rectRegex, line):
            width = int(m.group('width'))
            height = int(m.group('height'))

            for x in range(0, width):
                for y in range(0, height):
                    grid[y][x] = '#'
        elif m := re.match(rotateRowRegex, line):
            row = int(m.group('row'))
            by = int(m.group('by')) % WIDTH

            grid[row] = rightRotate(grid[row], by)

        elif m := re.match(rotateColRegex, line):
            col = int(m.group('col'))
            by = int(m.group('by')) % HEIGHT

            c = [grid[y][col] for y in range(0, HEIGHT)]
            c = rightRotate(c, by)
            for y in range(0, HEIGHT):
                grid[y][col] = c[y]
    return grid

def parseInput(stream):
    return render(stream.readlines())

def part1(grid):
    return len([cell for row in grid for cell in row if cell == '#'])

def part2(grid):
    return '\n' + '\n'.join([''.join(row) for row in grid])
