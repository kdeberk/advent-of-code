# 2016, Day 5.
# Input is a single word. For both parts, the answer is obtained by repeatedly generating the MD5 hash of the input
# followed by an increasing integer value.
#
# Part 1: For the first 8 generated MD5 hashes that have the prefix '00000', gather the first char following that prefix.
# Part 2: Until we've filled an 8-char string, use the 6th and 7th chars of the prefixed hashed to fill the places.

NAME = "Day 5: How About a Nice Game of Chess?"
SLOW = True

from md5 import MD5Generator

def parseInput(stream):
    return stream.read().strip()

def part1(input):
    p = ''
    g = MD5Generator(input, lambda h: h.startswith('00000'))
    while len(p) < 8:
        h, _ = g.next()
        p += h[5]
    return p

def part2(input):
    p = [None] * 8
    g = MD5Generator(input, lambda h: h.startswith('00000'))
    while None in p:
        h, _ = g.next()
        idx = int(h[5], base=16)
        if idx < len(p) and not p[idx]:
            p[idx] = h[6]
    return ''.join(p)
