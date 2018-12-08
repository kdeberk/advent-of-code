#!/usr/bin/env ruby

lines = File.read("input").split("\n")

node_names = []
children = []

lines.each do |line|
  match = /(.*) \((\d+)\)( -> (.*))?/.match(line)
  node_names << match[1]
  children += match[4].split(', ') if !match[4].nil?
end

root_name = (node_names - children)[0]
puts root_name

# extended puzzle

Node = Struct.new(:name, :weight, :children, :total_weight)
nodes = node_names.map {|name| [name, Node.new(name, 0.0, [])]}.to_h

def unbalanced?(node)
  node.children.any? do |child|
    1 < node.children.map(&:total_weight).uniq.length
  end
end

def calculate_total_weights(node)
  node.children.each {|child| calculate_total_weights(child)}
  node.total_weight =
      node.weight + node.children.map(&:total_weight).reduce(0, &:+)
end

lines.each do |line|
  match = /(.*) \((\d+)\)( -> (.*))?/.match(line)
  name = match[1]
  weight = match[2].to_i
  children = match[4].nil? ? [] : match[4].split(', ') 

  nodes[name].weight = weight
  nodes[name].children = children.map {|child_name| nodes[child_name]}
end

calculate_total_weights(nodes[root_name])
lowest_unbalanced = nodes.values.select {|node| unbalanced?(node)}.min_by(&:total_weight)

by_weight = lowest_unbalanced.children.group_by(&:total_weight)

correct_weight = by_weight.find {|_key, nodes| 1 < nodes.length}.first
wrong_weight = by_weight.find {|_key, nodes| 1 == nodes.length}.first
wrong_weight_node = by_weight[wrong_weight].first

puts wrong_weight_node.weight + (correct_weight - wrong_weight)
