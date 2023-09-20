#!/usr/bin/env ruby

Virus = Struct.new(:x, :y, :direction) do
  def left
    new_direction =
        {up: :left, right: :up, down: :right, left: :down}[direction]
    Virus.new(x, y, new_direction)
  end

  def right
    new_direction =
        {up: :right, right: :down, down: :left, left: :up}[direction]
    Virus.new(x, y, new_direction)
  end

  def reverse
    new_direction =
        {up: :down, right: :left, down: :up, left: :right}[direction]
    Virus.new(x, y, new_direction)
  end

  # @return [Virus]
  def move
    case direction
    when :up    then Virus.new(x, y - 1, direction)
    when :down  then Virus.new(x, y + 1, direction)
    when :left  then Virus.new(x - 1, y, direction)
    when :right then Virus.new(x + 1, y, direction)
    end
  end
end

def part_1(virus, infected)
  virus = virus.dup
  infected = infected.dup
  n_bursts = 0

  10_000.times do
    case infected[[virus.y, virus.x]]
    when nil
      infected[[virus.y, virus.x]] = :infected
      n_bursts += 1
      virus = virus.left.move
    when :infected
      infected[[virus.y, virus.x]] = nil
      virus = virus.right.move
    end
  end
  puts n_bursts
end

def part_2(virus, infected)
  virus = virus.dup
  infected = infected.dup
  n_bursts = 0

  10_000_000.times do
    case infected[[virus.y, virus.x]]
    when nil
      infected[[virus.y, virus.x]] = :weakened
      virus = virus.left.move
    when :weakened
      infected[[virus.y, virus.x]] = :infected
      virus = virus.move
      n_bursts += 1
    when :infected
      infected[[virus.y, virus.x]] = :flagged
      virus = virus.right.move
    when :flagged
      infected.delete([virus.y, virus.x])
      virus = virus.reverse.move
    end
  end
  puts n_bursts
end

infected = {}
grid = File.read('input').split("\n").map {|line| line.split('')}
grid.each_with_index do |line, y|
  line.each_with_index do |cell, x|
    infected[[y, x]] = :infected if '#' == grid[y][x]
  end
end

virus = Virus.new(grid.first.length / 2, grid.length / 2, :up)
part_1(virus, infected)
part_2(virus, infected)
