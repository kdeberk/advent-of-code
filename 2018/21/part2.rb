#!/usr/bin/env ruby

require 'set'

previous = nil
results = Set.new([])

c = 0
loop do
  b = c | 65536
  c = 1505483
  loop do
    d = b & 255
    c = (((c + d) & 16777215) * 65899) & 16777215
    if 256 > b
      if results.member?(c)
        puts previous
        exit
      else
        results.add(c)
        previous = c
        break
      end
    else
      b = b / 256
    end
  end
end
