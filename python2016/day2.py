# 2016, Day 2.
# Input is a set of lines consisting only of U, L, R and D chars. Imagine a finger over a
# keypad, each line controls the movement the finger and press the button once the end of the line is reached.
#
# Part 1: 3x3 grid keypad.
# Part 2: Diamond keypad with 13 buttons.

NAME = "Day 2: Bathroom Security"

moves = {
    'U': lambda x, y: [x-1, y  ],
    'L': lambda x, y: [  x, y-1],
    'R': lambda x, y: [  x, y+1],
    'D': lambda x, y: [x+1, y  ],
}

pad1 = [[1, 2, 3],
        [4, 5, 6],
        [7, 8, 9]]

pad2 = [[None, None,   1,  None, None],
        [None,   2 ,   3 ,   4 , None],
        [   5,   6 ,   7 ,   8 ,    9],
        [None,  'A',  'B',  'C', None],
        [None, None,  'D', None, None]]


def parseInput(stream):
    return stream.read().strip()

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
    return walkPad(input, pad1)

def part2(input):
    return walkPad(input, pad2)
