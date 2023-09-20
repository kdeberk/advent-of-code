#!/usr/bin/env ruby

n_muls = 0

a = 0  # 1 in part2
b = 79
c = b
if a == 1
  b *= 100
  b += 100_000
  c = b
  c += 17_000
end

loop do # 9
  f = 1
  d = 2
  loop do # 11
    e = 2
    loop do # 12
      g = d
      g *= e; n_muls += 1
      g -= b
      f = 0 if 0 == g
      e += 1
      g = e
      g -= b
      break if 0 == g
    end
    d += 1
    g = d
    g -= b
    break if 0 == g
  end
  h += 1 if 0 == f
  g = b
  g -= c

  break if 0 == g
  b += 17
end

puts n_muls
