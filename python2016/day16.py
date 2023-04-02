# 2016, Day 16.
# We fill a disk with non-random bits. This sequence of bits is constructed from
# a pattern that grows by reversion and inversion, using a method similar to the
# growth of the binary dragon curve.
#
# Approach: we don't need to expand the string, only generate its checksum. Each
# letter in the checksum represents a even-sized block in the expanded string and
# its value is purely controlled by whether the number of "1"s (or "0"s) in that
# block is even or odd.
#
# Such a block consists of several things that control its inverted parity.
# - a certain number n of AB pairs. Where A is the input and B is the reversed inversion of A.
# - a certain number of bits from the dragon fractal sprinkled throughout.
# - a prefix of an AB pair, the AB pair is then split between two checksum blocks.
# - a suffix of an AB pair, because the prefix is in the previous block.
#
# Part 1: Calculate the checksum for a disk of 242 bits.
# Part 2: Calculate the checksum for a disk of 35651584 bits.

# The parity of the first n digits of the dragon fractal.
# Apparently, no one knows how this works.
def dragonParity(n):
    gray = n ^ (n >> 1)
    return (gray ^ (n & gray).bit_count()) & 1

# Based on https://www.reddit.com/r/adventofcode/comments/5ititq/comment/dbbk6o9
def solve(pattern, diskSize):
    # Bit trick returns the largest power of 2 that divides l.
    # 272            = 100010000
    # 272-1          = 100001111
    # ~(272-1)       = 011110000
    # 272 & ~(272-1) = 000010000
    blockSize = diskSize & ~(diskSize-1)

    pair = pattern + [x ^ 1 for x in reversed(pattern)]

    sum = []
    prev = 0
    offset = blockSize
    while offset <= diskSize:
        nDragons = offset // (len(pattern)+1)
        nPairs   = (offset - nDragons) // (len(pair))
        prefix   = pair[:(offset - nDragons) % len(pair)]
        p = dragonParity(nDragons) # Parity of entire dragon fractal considered so far over all prev. blocks.
        p ^= nPairs & len(pattern) # Parity of n AB pairs is 1 if both len(pattern) and N are odd.
        p ^= prefix.count(1) % 2   # Parity of the prefix in this block.
        p &= 1                     # Only consider last bit.
        sum.append(p ^ prev ^ 1)

        offset += blockSize
        prev = p

    return ''.join([str(x) for x in sum])

def part1(input):
    return solve(input, 272)

def part2(input):
    return solve(input, 35651584)

if __name__ == "__main__":
    input = [int(x) for x in '10111011111001111']
    print("Part 1:", part1(input))
    print("Part 2:", part2(input))
