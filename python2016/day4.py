# 2016, Day 4.
# Input consists of encrypted room identifiers. Each contains a name, a digit and a checksum part.
#
# Part 1: Count the number of rooms with the correct checksum.
# Part 2: Decrypt the room identifiers and find the one named 'northpole object storage'.

import re
from itertools import groupby

lineRegex = r'^(?P<name>[a-z-]+)-(?P<id>[0-9]+)\[(?P<checksum>[a-z]+)\]'

def parseRoom(line: str):
    if m := re.search(lineRegex, line):
        room = m.groupdict()
        room['id'] = int(room['id'])
        return room
    assert False

def validChecksum(room):
    sum = ''.join([k for [k, v]
                   in sorted({c: room['name'].count(c)
                              for c in set(room['name'])
                              if c != '-'}.items(),
                             # -kv[1] makes count negative, we want to sort descending without
                             # using reverse=True
                             key=lambda kv: (-kv[1], kv[0]))[:5]])
    return sum == room['checksum']

def decryptRoom(room):
    alpha = "abcdefghijklmnopqrstuvwxyz"
    return ''.join([' ' if c == '-'
                    else alpha[(alpha.index(c)+room['id'])%len(alpha)]
                    for c in room['name']])

def part1(lines):
    return sum(room['id']
               for room in [parseRoom(line) for line in lines]
               if validChecksum(room))

def part2(lines):
    for line in lines:
        room = parseRoom(line)
        if validChecksum(room) and 'northpole object storage' == decryptRoom(room):
            return room['id']


if __name__ == "__main__":
    input = open("input/day4.txt").readlines()
    print("Part 1:", part1(input))
    print("Part 2:", part2(input))
