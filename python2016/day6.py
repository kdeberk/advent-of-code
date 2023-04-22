# 2016, Day 6.
# Input forms a table. Each line has same length and contains only letters.
#
# Part 1: For each column, find the most occurring letter.
# Part 2: For each column, find the least occurring letter.

NAME = "Day 6: Signals and Noise"

def parseInput(stream):
    lines = [l.strip() for l in stream.readlines()]
    counts = [dict() for _ in range(0, len(lines[0]))]
    for line in lines:
        for idx, c in enumerate(line):
            if c not in counts[idx]:
                counts[idx][c] = 0
            counts[idx][c]+= 1
    return counts


def part1(counts):
    return ''.join(kv[0] for kv in [max(count.items(), key=lambda kv: kv[1])
                                    for count in counts])

def part2(counts):
    return ''.join(kv[0] for kv in [min(count.items(), key=lambda kv: kv[1])
                                    for count in counts])
