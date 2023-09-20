#!/usr/bin/env ruby

components = File.read('input').split("\n").map {|l| l.split('/').map(&:to_i)}

Chain = Struct.new(:components, :last_port_type) do
  def strength
    components.flatten.reduce(&:+)
  end

  def length
    components.length
  end

  def with_added_component(component)
    Chain.new(components + [component],
              component.first == last_port_type ? component[1] : component.first)
  end
end

def strongest_bridge(chain, components_left)
  possible = components_left.select do |component|
    component.include?(chain.last_port_type)
  end

  if possible.empty?
    chain
  else
    possible.flat_map do |next_component|
      new_components_left = components_left.dup.tap {|c| c.delete(next_component)}

      strongest_bridge(chain.with_added_component(next_component), new_components_left)
    end.reduce do |best, current|
      if best.strength > current.strength
        best
      else
        current
      end
    end
  end
end

def longest_bridge(chain, components_left)
  possible = components_left.select do |component|
    component.include?(chain.last_port_type)
  end

  if possible.empty?
    chain
  else
    possible.flat_map do |next_component|
      new_components_left = components_left.dup.tap {|c| c.delete(next_component)}

      longest_bridge(chain.with_added_component(next_component), new_components_left)
    end.reduce do |best, current|
      if best.length > current.length
        best
      elsif best.length == current.length && best.strength > current.strength
        best
      else
        current
      end
    end
  end
end

puts strongest_bridge(Chain.new([], 0), components).strength
puts longest_bridge(Chain.new([], 0), components).strength
