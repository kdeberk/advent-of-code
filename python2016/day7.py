# 2016, Day 7.
# The input lines contain IPv7 addresses which each consist of 'inner' and 'outer' blocks.
# These blocks can contain an ABBA or ABA substrings.
#
# Part 1: Count addresses for which at least one outer has an ABBA but no inner has one.
# Part 2: Count addresses for which an outer has an ABA and an inner has an associated BAB.

NAME = "Day 7: Internet Protocol Version 7"

import re

def parseInput(stream):
    return stream.readlines()

def splitBlocks(line):
    inners = []
    outers = []
    for idx, block in enumerate(re.split(r'[\[\]]', line.strip())):
        if idx % 2 == 0:
            outers.append(block)
        else:
            inners.append(block)
    return [inners, outers]

def containsABBA(txt):
    for idx in range(0, len(txt)-3):
        a, b, c, d = txt[idx:idx+4]
        if a == d and b == c and a != b:
            return True
    return False

def listABAs(txt):
    for idx in range(0, len(txt)-2):
        a, b, c = txt[idx:idx+3]
        if a == c and a != b:
            yield txt[idx:idx+3]

def invertABA(aba):
    return aba[1] + aba[0] + aba[1]

def part1(lines):
    count = 0
    for line in lines:
        inners, outers = splitBlocks(line.strip())
        if any(filter(containsABBA, outers)) and not any(filter(containsABBA, inners)):
            count += 1
    return count

def part2(lines):
    count = 0
    for line in lines:
        inners, outers = splitBlocks(line.strip())
        for bab in [invertABA(aba) for outer in outers for aba in listABAs(outer)]:
            if any(filter(lambda inner: bab in inner, inners)):
                count += 1
                break

    return count
