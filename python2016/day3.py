# 2016, Day 3.
# Input is a table of numbers with 3 columns.
#
# Part 1: Count the rows that form a possible set of triangle numbers.
# Part 2: For each group of three lines, count the number of columns
#   (of size 3) that form a possible set of triangle numbers.

NAME = "Day 3: Squares With Three Sides"

def trianglish(a, b, c):
    return a+b > c and a+c > b and b+c > a

def parseInput(stream):
    return [[int(x) for x in line.split()] for line in stream.readlines()]

def part1(rows):
    return len([1 for [a, b, c] in rows
                if trianglish(a, b, c)])

def part2(rows):
    count = 0
    for row in range(0, len(rows), 3):
        for col in range(3):
            if trianglish(rows[row+0][col],
                          rows[row+1][col],
                          rows[row+2][col]):
                count += 1
    return count
