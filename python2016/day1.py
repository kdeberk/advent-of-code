NORTH = lambda x, y, d : [x + d, y]
SOUTH = lambda x, y, d : [x - d, y]
WEST = lambda x, y, d : [x, y - d]
EAST = lambda x, y, d : [x, y + d]

LEFT  = {NORTH: WEST, WEST: SOUTH, SOUTH: EAST, EAST: NORTH}
RIGHT = {NORTH: EAST, EAST: SOUTH, SOUTH: WEST, WEST: NORTH}


def part1(input):
    x, y, z = [0, 0, NORTH]
    for m in input.split(", "):
        if m[0] == 'L':
            z = LEFT[z]
        else:
            z = RIGHT[z]
        x, y = z(x, y, int(m[1:]))
    return abs(x)+abs(y)

def part2(input):
    seen = set()

    x, y, z = [0, 0, NORTH]
    for m in input.split(", "):
        if m[0] == 'L':
            z = LEFT[z]
        else:
            z = RIGHT[z]

        # Visit each block along the way.
        for _ in range(int(m[1:])):
            k = f'{x}-{y}'
            if k in seen:
                return abs(x)+abs(y)
            seen.add(k)
            x, y = z(x, y, 1)


if __name__ == "__main__":
    input = open("input/day1.txt").read().strip()

    print("Part 1:", part1(input))
    print("Part 2:", part2(input))
