#!/usr/bin/env ruby

banks = [0, 5, 10, 0, 11, 14, 13, 4, 11, 8, 8, 7, 1, 4, 12, 11]

count = 0
seen = [banks.join('')]

until count > 0 && seen.include?(banks.join(''))
  seen << banks.join('')

  max = banks.max
  index = banks.index(max)

  banks[index] = 0
  index = (index + 1) % banks.length
  until 0 == max
    banks[index] += 1
    max -= 1
    index = (index + 1) % banks.length
  end

  count += 1
end

puts count
puts seen.length - seen.index(banks.join(''))
