# 2016, Day 9.
# Data contains a compressed string, where consecutive repeated substrings are replaced with a block consisting of a
# `(MxN)` marker followed by a single copy of the substring. The string contains multiple of these markers and these
# blocks can be nested. The final string can be huge but only the size of the final string is needed. The approach
# is to count the sizes of the decompressed blocks without decompressing.
#
# Part 1: Ignore nested markers.
# Part 2: Don't ignore nested markers. First recurse on the substring to determine the final length of decompressing
#   the substring, and then apply the marker.

NAME = "Day 9: Explosives in Cyberspace"

def parseInput(stream):
    return stream.read().strip()

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
        if m := rdr.readMarker():
            rdr.read(m[0])
            count += m[0] * m[1]
        else:
            rdr.read(1)
            count += 1
    return count

def part2(input):
    rdr = Reader(input)

    count = 0
    while not rdr.done():
        if m := rdr.readMarker():
            count += m[1] * part2(rdr.read(m[0]))
        else:
            rdr.read(1)
            count += 1
    return count
