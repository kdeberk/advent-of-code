#!/usr/bin/env ruby

count = 0
current = 107_900
final = 124_900
loop do
  for j in 2..Math.sqrt(current) do
    if 0 == current % j
      count += 1
      break
    end
  end
  current += 17
  break if current > final
end
puts count

# a = 1  # 1 in part2
# if a == 0
#   b = c = 79
# elsif a == 1
#   b = 107_900
# end

# h = 0

# loop do # 9
#   f = false
#   d = 2
#   while !f  # 11
#     e = 2
#     while !f # 12
#       f = true if d * e == b
#       e += 1
#       break if e == b
#     end
#     d += 1
#     break if d == b
#   end
#   h += 1 if true == f

#   break if b == 124_900
#   b += 17
# end

# puts h
