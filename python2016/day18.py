# 2016, Day 18
# The floor is covered with mined tiles. The positions of mined tiles on one row
# determine the positions of mined tile on the next row so given only the locations
# of the mines on the first row, we can locate the mined tiles for all rows on the floor.
#
# Approach: a row can be represented by a binary string where ^ becomes 1 and . becomes 0.
# The reproduction rule states that a tile is only mined if the three tiles on the previous
# row are 001, 011, 100, or 110. The value of the middle tile is not important and a tile
# is only mined if the first and last tiles have different values.
#
# This allows us to generate a whole row without looking at each tile individually with the
# expression `row>>1 ^ row<<1` and then handling the border tiles.
#
# Part 1: Number of safe tiles on first 40 rows.
# Part 2: Number of safe tiles on first 400_000 rows.

def parseInput(line):
    return int("".join(["1" if c == '^' else "0" for c in line]), 2)

def nextRow(row, width):
    row |= 2**(width+1) # Add a single on-bit to guard the left side
    row = row >> 1 ^ row << 1
    row &= (2**width)-1
    return row

def countSafeTiles(row, width, nRows):
    count = row.bit_count()
    for _ in range(1, nRows):
        row = nextRow(row, width)
        count += row.bit_count()
    return (nRows * width) - count

def part1(input):
    return countSafeTiles(input, 100, 40)

def part2(input):
    return countSafeTiles(input, 100, 400_000)

if __name__ == "__main__":
    input = parseInput(open("input/day18.txt").read().strip())
    print("Part 1:", part1(input))
    print("Part 2:", part2(input))
