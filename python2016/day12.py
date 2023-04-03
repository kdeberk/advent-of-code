# 2016, Day 12.
# Input is a program in assembunny. Instead of writing an emulator, we manually decode
# the instructions to python and execute that.

NAME = "Day 12: Leonardo's Monorail"

# cpy 1 a      a, b, c, d = 1, 1, 0, 26
# cpy 1 b
# cpy 26 d
#
# jnz c 2      goto A if 0 < c
# jnz 1 5      goto B
# cpy 7 c  A   c = 7                       d += 7
# inc d    C   d += 1              d += c
# dec c        c -= 1
# jnz c -2     goto C if 0 < c
#
# cpy a c  B   c = a                       fib(d)
# inc a    D   a += 1              a += b
# dec b        b -= 1
# jnz b -2     goto D if 0 < b
# cpy c b      b = c
# dec d        d -= 1
# jnz d -6     goto B if 0 < d
#
# cpy 14 c     c = 14                      a += 14 * 14
# cpy 14 d F   d = 14
# inc a    E   a += 1              a += d
# dec d        d -= 1
# jnz d -2     goto E if 0 < d
# dec c        c -= 1
# jnz c -5     goto F if 0 < c

def fib(n, a, b):
    if 0 <= n:
        return fib(n - 1, b, a+b)
    return a

def part1():
    return fib(26, 1, 1) + 14 * 14

def part2():
    return fib(26+7, 1, 1) + 14 * 14
