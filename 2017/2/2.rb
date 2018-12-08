#!/usr/bin/env ruby

lines = File.read('input').split("\n").map {|line| line.split(/\s+/).map(&:strip).map(&:to_i)}

div_sum = 0
min_max_sum = 0

lines.each do |line|
  min_max_sum += line.max - line.min

  for i in 0..(line.length - 1)
    for j in (i + 1)...line.length
      if 0 == line[i] % line[j]
        puts "%d / %d" % [line[i], line[j]]
        div_sum += line[i] / line[j]
        break 2
      elsif 0 == line[j] % line[i]
        puts "%d / %d" % [line[j], line[i]]
        div_sum += line[j] / line[i]
        break 2
      end
    end
  end
end

puts min_max_sum
puts div_sum
