def calcCounts(lines):
    lines = [l.strip() for l in lines]
    counts = [dict() for col in range(0, len(lines[0]))]
    for line in lines:
        for idx, c in enumerate(line):
            if c not in counts[idx]:
                counts[idx][c] = 0
            counts[idx][c]+= 1
    return counts


def part1(counts):
    return ''.join(kv[0] for kv in [max(count.items(), key=lambda kv: kv[1])
                                    for count in counts])

def part2(input):
    return ''.join(kv[0] for kv in [min(count.items(), key=lambda kv: kv[1])
                                    for count in counts])

if __name__ == "__main__":
    counts = calcCounts(open("input/day6.txt").readlines())
    print("Part 1:", part1(counts))
    print("Part 2:", part2(counts))
