# 2016, Day 23.
# Similar to Day 12, the input is an assembunny program. Program was again manually decoded.
#
# The program encodes the factorial algorithm.

NAME = "Day 23: Safe Cracking"

# cpy a b    a = b = 7
# dec b      b -= 1

# cpy a d    d = a                         a *= b
# cpy 0 a    a = 0
# cpy b c    c = b              a = b*d
# inc a      a += 1    a += c
# dec c      c -= 1
# jnz c -2
# dec d      d -= 1
# jnz d -5

# dec b      b -= 1             c = 2*(--b)

# cpy b c    c = d = b
# cpy c d
# dec d      d -= 1    c += d
# inc c      c += 1
# jnz d -2

# Above code block is executed multiple times.
# Before: a = 7, b = 6, c = 0, d = 0
# 1st:    a = 42, b = 5, c = 10, d = 0
# 2nd:    a = 210, b = 4, c = 8, d = 0
# 3rd:    a = 840, b = 3, c = 6, d = 0
# 4th:    a = 2520, b = 2, c = 4, d = 0
# 5th:    a = 5040, b = 1, c = 2, d = 0

# tgl c
# cpy -16 c
# cpy 1 c    c = 1
# cpy 89 c   c = 89
# cpy 79 d   d = 79

# inc a      a += 1   a += d   a += c*d
# dec d      d -= 1
# jnz d -2
# dec c      c -= 1
# jnz c -5

from functools import reduce

def fact(n):
    return reduce(lambda acc, n: acc * n, range(1, n+1))

def part1():
    return fact(7) + 79*89

def part2():
    return fact(12) + 79*89
