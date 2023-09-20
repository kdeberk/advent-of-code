#!/usr/bin/env ruby

key = 'wenycdww'


hashes = 128.times.map do |row|
  chain = (0..255).to_a
  n_moved = 0
  skip_size = 0

  input = ("%s-%d" % [key, row]).bytes + [17, 31, 73, 47, 23]
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
  chain.each_slice(16).map {|slice| slice.reduce(&:^)}
end

n_used_cells = 0
hashes.each do |hash|
  hash.each do |byte|
    until byte.zero?
      n_used_cells += 1 if 1 == byte % 2
      byte /= 2
    end
  end
end

puts n_used_cells

# part 2

Cell = Struct.new(:row, :column, :filled, :group)
grid = hashes.each_with_index.map do |hash, row|
  bits = (("%08b" * 16) % hash).split('')

  bits.each_with_index.map do |v, column|
    Cell.new(row, column, v == '1', nil)
  end
end

def cascade_group(cell, grid)
  to_do = [cell]

  until to_do.empty?
    cell = to_do.shift

    if 0 < cell.row
      up = grid[cell.row - 1][cell.column]
      raise if !up.group.nil? && up.group != cell.group
      if up.filled && up.group.nil?
        up.group = cell.group
        to_do << up
      end
    end

    if 0 < cell.column
      left = grid[cell.row][cell.column - 1]
      raise if !left.group.nil? && left.group != cell.group
      if left.filled && left.group.nil?
        left.group = cell.group
        to_do << left
      end
    end

    if 127 > cell.row
      down = grid[cell.row + 1][cell.column]
      raise if !down.group.nil? && down.group != cell.group
      if down.filled && down.group.nil?
        down.group = cell.group
        to_do << down
      end
    end

    if 127 > cell.column
      right = grid[cell.row][cell.column + 1]
      raise if !right.group.nil? && right.group != cell.group
      if right.filled && right.group.nil?
        right.group = cell.group
        to_do << right
      end
    end
  end
end

n_groups_found = 0
grid.each do |row|
  row.each do |cell|
    if true == cell.filled && nil == cell.group
      n_groups_found += 1
      cell.group = n_groups_found
      cascade_group(cell, grid)
    end
  end
end

puts n_groups_found
