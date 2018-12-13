#!/usr/bin/env ruby

list = [0]
n_steps = 363

(1..2017).each do |i|
  list = list[(n_steps % list.length)..-1] + list[0...(n_steps % list.length)]
  list = [i] + list[1..-1] + [list[0]]
end

puts list[1]

index = 0
length = 1
last_n = nil

(1..50*(10**6)).each do |i| 
  index = (index + n_steps) % length
  length += 1
  last_n = i if 0 == index
  index += 1
end

puts last_n
