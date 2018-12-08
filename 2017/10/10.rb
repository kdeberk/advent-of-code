#!/usr/bin/env ruby

input = [192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12]
chain = (0..255).to_a

n_moved = 0
skip_size = 0
input.each do |i|
  chain += chain.shift(i).reverse 
  chain += chain.shift(skip_size)

  n_moved += i + skip_size
  skip_size += 1
end

to_correct = chain.length - (n_moved % chain.length)
chain += chain.shift(to_correct)

puts chain[0] * chain[1]

## extended puzzle

input = '192,69,168,160,78,1,166,28,0,83,198,2,254,255,41,12'
chain = (0..255).to_a

input = input.bytes + [17, 31, 73, 47, 23]

n_moved = 0
skip_size = 0
64.times do
  input.each do |i|
    chain += chain.shift(i).reverse 
    chain += chain.shift(skip_size % chain.length)

    n_moved += i + skip_size
    skip_size += 1
  end
end

to_correct = chain.length - (n_moved % chain.length)
chain += chain.shift(to_correct)

dense_hash = chain.each_slice(16).map {|slice| "%02x" % [slice.reduce(&:^)]}

puts dense_hash.join('')
