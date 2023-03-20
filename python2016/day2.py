moves = {
    'U': lambda x, y: [x-1, y  ],
    'L': lambda x, y: [  x, y-1],
    'R': lambda x, y: [  x, y+1],
    'D': lambda x, y: [x+1, y  ],
}

def findFive(pad):
    for x, row in enumerate(pad):
        for y, cell in enumerate(row):
            if cell == 5:
                return x, y
    assert False

def walkPad(route, pad):
    x, y = findFive(pad)

    code = ''
    for c in route:
        if c == '\n':
            code += str(pad[x][y])
        else:
            nx, ny = moves[c](x, y)
            if 0 <= nx and nx < len(pad) and 0 <= ny and ny < len(pad[nx]) and pad[nx][ny]:
                x, y = nx, ny
    return code + str(pad[x][y])

def part1(input):
    return walkPad(input, [[1, 2, 3], [4, 5, 6], [7, 8, 9]])

def part2(input):
    return walkPad(input, [[None, None, 1, None, None], [None, 2, 3, 4, None], [5, 6, 7, 8, 9], [None, 'A', 'B', 'C', None], [None, None, 'D', None, None]])

if __name__ == "__main__":
    input = open("input/day2.txt").read().strip()

    print("Part 1:", part1(input))
    print("Part 2:", part2(input))
