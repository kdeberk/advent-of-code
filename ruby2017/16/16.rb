#!/usr/bin/env ruby

dance = File.read('input').split(',')

line = 'abcdefghijklmnop'
dance.each do |move|
  op, operands = [move[0], move[1..-1]]
  case op
  when 's'
    operands.to_i.times {line = line[-1] + line[0..-2]}
  when 'p'
    a, b = operands.split('/').map {|c| line.index(c)}
    x, y = line[a], line[b]
    line[a], line[b] = y, x
  when 'x'
    a, b = operands.split('/').map(&:to_i)
    x, y = line[a], line[b]
    line[a], line[b] = y, x
  end
end

puts line

line = 'abcdefghijklmnop'
(10**9 % 30).times do |n_iter|
  dance.each do |move|
    op, operands = [move[0], move[1..-1]]
    case op
    when 's'
      operands.to_i.times {line = line[-1] + line[0..-2]}
    when 'p'
      a, b = operands.split('/').map {|c| line.index(c)}
      x, y = line[a], line[b]
      line[a], line[b] = y, x
    when 'x'
      a, b = operands.split('/').map(&:to_i)
      x, y = line[a], line[b]
      line[a], line[b] = y, x
    end
  end
end

puts line
