#!/usr/bin/env ruby

# thoughts

# 883 is prime
# 16807 = 7**5
# 879 = 3 * 293
# 48271 is prime
# 2147483647 = 2**31 - 1

factor_a, factor_b = 16807, 48271
modulo = 0x7fffffff
mask = 0xffff

count = 0
current_a, current_b = 883, 879
40_000_000.times do
  count += 1 if current_a & mask == current_b & mask
  
  current_a = (current_a * factor_a) % modulo
  current_b = (current_b * factor_b) % modulo
end

puts count


count = 0
current_a, current_b = 883, 879
5_000_000.times do
  count += 1 if current_a & mask == current_b & mask

  current_a = (current_a * factor_a) % modulo
  current_a = (current_a * factor_a) % modulo until 0 == current_a % 4

  current_b = (current_b * factor_b) % modulo
  current_b = (current_b * factor_b) % modulo until 0 == current_b % 8
end

puts count
