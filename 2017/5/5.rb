#!/usr/bin/env ruby

jumps = File.read('input').split("\n").map(&:to_i)

count = 0
current = 0
until current >= jumps.length
  next_ = current + jumps[current]
  jumps[current] += 1

  current = next_
  count += 1
end
puts count


jumps = File.read('input').split("\n").map(&:to_i)

count = 0
current = 0
until current >= jumps.length
  next_ = current + jumps[current]
  if jumps[current] >= 3
    jumps[current] -= 1
  else
    jumps[current] += 1
  end

  current = next_
  count += 1
end
puts count
