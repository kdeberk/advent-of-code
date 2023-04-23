# 2016, Day 25.
# Final puzzle offers another assembunny program.
#
# The program takes an input to produce a certain number, then prints the bits
#  in that number and restarts once it has reached the end.
#
# Part 1: We obtain the value of a by subtracting 2548 from the smallest number
#   larger than 2548 that consists of a repeated binary 10 pattern.

NAME = "Day 25: Clock Signal"

#   cpy a d    d = a                       d = a+2548
#   cpy 14 c   c = 14
#B  cpy 182 b  b = 182          d += 182c
#A  inc d      d += 1   d += b
#   dec b      b -= 1
#   jnz b A
#   dec c      c -= 1
#   jnz c B

# Following blocks are repeated, each iteration starts with same value of d.
#H  cpy d a    a = d
#G  jnz 0 0
#   cpy a b    b = a   b = a, a = 0
#   cpy 0 a    a = 0

# Following block sets a = b//2, c is final bit and calls next block with c.
#E  cpy 2 c    c = 2
#D  jnz b C
#   jnz 1 I    jmp     b==0
#C  dec b      b -= 1  0<b
#   dec c      c -= 1  0<b
#   jnz c D            0<b
#   inc a      a += 1  0<b && c==0
#   jnz 1 E    jmp     0<b && 0<c

# Following block emits 0 if c = 2 and 1 if c = 1
#I  cpy 2 b    b = 2
#F  jnz c 2                 while 0<c,b-=1,c-=1 or b-=c
#   jnz 1 J    jmp     c==0
#   dec b      b -= 1  0<c
#   dec c      c -= 1  0<c
#   jnz 1 F    jmp     0<c

#J  jnz 0 0
#   out b
#   jnz a G
#   jnz 1 H    jmp

def part1():
    x = 2548
    b = 0
    while b < x:
        b = b * 4 + 2
    return b - x
