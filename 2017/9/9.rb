#!/usr/bin/env ruby

stream = File.read('input')

depth = 0
sum_of_depths = 0
n_removed_chars = 0

in_garbage = false
ignore_next = false

stream.chars.each do |char|
  if ignore_next
    ignore_next = false
    next
  elsif in_garbage && char != '!' && char != '>'
    n_removed_chars += 1    
  end

  case char
  when '{'
    next if in_garbage

    depth += 1
    sum_of_depths += depth
  when '}'
    next if in_garbage

    depth -= 1
  when '!'
    ignore_next = true
  when '<'
    in_garbage = true
  when '>'
    in_garbage = false
  end
end

puts sum_of_depths
puts n_removed_chars
