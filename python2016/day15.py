# 2016, Day 15.
# A capsule needs to fall through a series of rotating discs. Each has a single hole and has a prime number of
#  orientations.
# The approach is as follows:
# - For each disc, calculate the first time point t_0 for which it's at position 0.
#   The solution matches t_0 + pn where p is the number of positions for that disc and n is
#   some unknown positive integer.
# - Subtract idx+1 from that t_0, since the ball needs to travel for idx+1 to reach that disc.
# - Starting at t_0 for the largest disc, keep adding p until it aligns with the second-largest disc.
#   p then becomes the product of both disc sizes. Keep adding until all discs align.
#
# Part 1: Calculate the earliest possible time for 6 discs.
# Part 2: Calculate the earliest possible time for 7 discs.

NAME = "Day 15: Timing is Everything"

import re

discRegex = r'Disc #\d has (?P<npos>\d+) positions; at time=0, it is at position (?P<pos>\d+)'

def parseInput(stream):
    discs = []
    for l in stream.readlines():
        if m := re.match(discRegex, l):
            discs.append({k: int(v) for (k, v) in m.groupdict().items()})
    return discs

def calcDropTime(discs):
    pos = []
    for idx, disc in enumerate(discs):
        # We need to rotate each disc from its current position until we get a time point at which it's at position 0.
        # Then we use the index-1 to calculate the earlier time at which the capsule is dropped.
        pos.append(((disc['npos'] - disc['pos'] - (idx+1)) % disc['npos'], disc['npos']))
    # Sort by disc size, largest first.
    pos.sort(key=lambda x: x[1], reverse=True)

    (t, npos1) = pos[0]
    for (t_, npos2) in pos[1:]:
        while t % npos2 != t_:
            t += npos1
        # N.B. Multiplication doesn't work if the numbers aren't coprime (then we need to use LCM), but since they're
        # all prime, we can get away with it.
        npos1 *= npos2
    return t

def part1(discs):
    return calcDropTime(discs)

def part2(discs):
    return calcDropTime(discs + [{'npos': 11, 'pos': 0}])
