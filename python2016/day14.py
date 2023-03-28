# 2016, Day 14.
# Similar to day 5: the solution requires us to generate md5 hashes for a fixed prefix
# followed by an incrementing integer value. We want to obtain the 64th hash that contains
# the same char 3 times in sequece for which that same char appears 5 times in sequence in
# a later hash that is 1000 or less increments away.
#
# Approach: Whenever we find a 3-hash, we store the index in a FIFO queue for that char.
#  Whenever we find a 5-hash for any char, we gather the 3-hashes that were generated less
#  than 1000 increments ago.
# We only know whether we found the 64th once we've gone beyond 1000 increments for the
# candidate for the 64th key, so we continue a bit after we've found 64 keys.
#
# Part 1: Find the 64th key for which MD5(prefix + n) contains the triple and
#   MD5(prefix + m) for some m <= n + 1000 contains a quintuple.
# Part 2: Find the 64th key with the same properties as with part 1, but after applying
#   MD5 2017 times.

from md5 import MD5Generator

def threeInARow(h):
    for idx in range(0, len(h) - 2):
        if h[idx] == h[idx+1] and h[idx] == h[idx+2]:
            return h[idx]
    return None

def fiveInARow(h):
    cs = set()
    for idx in range(0, len(h) - 4):
        if h[idx] == h[idx+1] and h[idx] == h[idx+2] and h[idx] == h[idx+3] and h[idx] == h[idx+4]:
            cs.add(h[idx])
    return cs

def findMatches(g):
    triples = {c: [] for c in '0123456789abcdef'} # map hex chars to indexes that contained that char as a triple.

    keys = []
    while True:
        h, idx = g.next()

        for c in triples:
            # delete triple entries that have gone out of scope.
            while 0 < len(triples[c]) and triples[c][0] + 1000 < idx:
                triples[c].pop(0)

        if c := threeInARow(h):
            triples[c].append(idx)

        for c in fiveInARow(h):
            while 0 < len(triples[c]) and triples[c][0] < idx:
                keys.append(triples[c].pop(0))

        # wait for the 64th candidate to go out of scope before we return.
        keys.sort()
        if 64 <= len(keys) and keys[63] + 1000 < idx:
            return keys[63]

def part1(input):
    return findMatches(MD5Generator(input, threeInARow))

def part2(input):
    return findMatches(MD5Generator(input, threeInARow, 2017))

if __name__ == "__main__":
    input = 'ngcjuoqr'
    print("Part 1:", part1(input))
    print("Part 2:", part2(input))
