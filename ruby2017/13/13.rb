#!/usr/bin/env ruby

lines = File.read('input').split("\n")

layers = lines.map {|line| line.split(':').map(&:to_i)}

puts layers
       .select {|(depth, range)| 0 == depth % (2 * (range - 1))}
       .map{|(depth, range)| depth * range}
       .reduce(&:+)


# the delays at which we can safely pass through the firewall are expressed as the first number that cannot be expressed as

# 2 * i * (range - 1) - depth

# for some i, and range, depth of every layer

delay = 0
loop do
  hit = false

  for (depth, range) in layers do
    if 0 == (delay + depth) % (2 * (range - 1))
      hit = true
      break
    end
  end

  break unless hit
  delay += 1
end

puts delay
