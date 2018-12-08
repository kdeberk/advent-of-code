#!/usr/bin/env ruby

lines = File.read('input').split("\n")

Node = Struct.new(:i, :neighbors, :group)

nodes = {}

lines.each do |line|
  match = line.match(/(\d+) <-> ([\d,\s]+)/)
  node_i = match[1]
  neighbors = match[2]

  if !nodes.key?(node_i)
    nodes[node_i] = Node.new(node_i, [], nil)
  end

  nodes[node_i].neighbors = neighbors.split(', ')
end

while nodes.values.any? {|node| node.group.nil?}
  group_leader = nodes.values.select {|node| node.group.nil?}.min_by(&:i)

  group_leader.group = group_leader.i
  group_queue = [group_leader]

  until group_queue.empty?
    node = group_queue.shift

    node.neighbors.each do |neighbor_i|
      neighbor = nodes[neighbor_i]
      next unless neighbor.group.nil?

      neighbor.group = node.group
      group_queue << neighbor
    end
  end
end

puts nodes.values.select {|node| '0' == node.group}.length
puts nodes.values.map(&:group).uniq.length
