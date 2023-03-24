# 2016, Day 3.
# Input is a table of numbers with 3 columns.
#
# Part 1: Count the rows that form a possible set of triangle numbers.
# Part 2: For each group of three lines, count the number of columns
#   (of size 3) that form a possible set of triangle numbers.

def trianglish(a, b, c):
    return a+b > c and a+c > b and b+c > a

def part1(input):
    count = 0
    for row in input.split("\n"):
        [a, b, c] = [int(x) for x in row.split()]
        if trianglish(a, b, c):
            count += 1
    return count

def part2(input):
    count = 0
    rows = [[int(x) for x in row.split()]
            for row in input.split("\n")]
    for row in range(0, len(rows), 3):
        for col in range(3):
            if trianglish(rows[row+0][col],
                          rows[row+1][col],
                          rows[row+2][col]):
                count += 1
    return count

if __name__ == "__main__":
    input = open("input/day3.txt").read().strip()

    print("Part 1:", part1(input))
    print("Part 2:", part2(input))
