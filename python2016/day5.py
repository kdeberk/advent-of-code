# 2016, Day 5.
# Input is a single word. For both parts, the answer is obtained by repeatedly generating the MD5 hash of the input
# followed by an increasing integer value.
#
# Part 1: For the first 8 generated MD5 hashes that have the prefix '00000', gather the chars following that prefix.
# Part 2: Until we've filled an 8-char string, use the 6th and 7th chars of the prefixed hashed to fill the places.

import hashlib

input = 'cxdnnyjw'

class Generator(object):
    # List value [] is default so that all instances of Generator will share it.
    def __init__(self, l=[]):
        self.idx = 0
        self.n = 0
        self.l = l

    def next(self):
        if self.idx < len(self.l):
            self.idx += 1
            return self.l[self.idx-1]

        while True:
            h = hashlib.md5((input + str(self.n)).encode('utf-8')).hexdigest()
            self.n += 1

            if h.startswith('00000'):
                self.idx += 1
                self.l.append(h)
                return  h


def part1():
    p = ''
    g = Generator()
    while len(p) < 8:
        p += g.next()[5]
    return p

def part2():
    p = [None] * 8
    g = Generator()
    while None in p:
        h = g.next()
        idx = int(h[5], base=16)
        if idx < len(p) and not p[idx]:
            p[idx] = h[6]
    return ''.join(p)


if __name__ == "__main__":
    print("Part 1:", part1())
    print("Part 2:", part2())
