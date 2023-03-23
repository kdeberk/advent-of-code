# 2016, Day 9.
# Data contains a compressed string, where consecutive repeated substrings are replaced
# with a `(MxN)` marker and a single copy of the substring. The string contains multiple
# of these markers and these markers can be nested.
#
# Part 1: Ignore nested markers.
# Part 2: Decompress the nested substring and then apply the marker.


class Reader:
    def __init__(self, string):
        self.str = string
        self.idx = 0

    def done(self):
        return self.idx >= len(self.str)

    def read(self, n):
        self.idx += n
        return self.str[self.idx - n:self.idx]

    def readInt(self):
        n = 0
        while self.str[self.idx] in '0123456789':
            n = n * 10 + int(self.str[self.idx])
            self.idx += 1
        return n

    def readMarker(self):
        if not self.str[self.idx] == '(':
            return None
        self.read(1) # (
        n = self.readInt()
        self.read(1) # x
        m = self.readInt()
        self.read(1) # )
        return [n, m]


def part1(input):
    rdr = Reader(input)

    count = 0
    while not rdr.done():
        if b := rdr.readMarker():
            rdr.read(b[0])
            count += b[0] * b[1]
        else:
            rdr.read(1)
            count += 1
    return count

def part2(input):
    rdr = Reader(input)

    count = 0
    while not rdr.done():
        if b := rdr.readMarker():
            count += b[1] * part2(rdr.read(b[0]))
        else:
            rdr.read(1)
            count += 1
    return count

if __name__ == "__main__":
    input = open("input/day9.txt").read().strip()

    print("Part 1:", part1(input))
    print("Part 2:", part2(input))
