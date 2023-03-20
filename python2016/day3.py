
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
