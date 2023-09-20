#!/usr/bin/env ruby

# Takes a string of N lines, N characters each. Lines separated by \. Returns an
# array with N/n rows, each containing a string of n rows and n characters.
# @param [String] grid
# @param [Integer] n
# @return [<<String>>]
def split(grid, n)
  grid.split('/').each_slice(n).map do |rows|
    results = Array.new(rows.first.length / n) {[]}
    rows.map {|row| row.split('').each_slice(n).each_with_index {|r, i| results[i] << r.join('')}}
    results.map {|r| r.join('/')}
  end
end

def rotate(grid)
  rows = grid.split('/')
  rows.length.times.map do |i|
    rows.map {|row| row[i]}.reverse.join
  end.join('/')
end

def hflip(grid)
  grid.split('/').map(&:reverse).join('/')
end

def vflip(grid)
  grid.split('/').reverse.join('/')
end

def enhance(cell, book)
  rotations = [cell, rotate(cell), rotate(rotate(cell)), rotate(rotate(rotate(cell)))]
  flips = rotations.flat_map {|r| [r, vflip(r), hflip(r)]}

  found = flips.find {|flip| book.key?(flip)}
  raise if !found
  book[found]
end

def join(cells)
  return cells.flatten.first if 1 == cells.flatten.length

  grid = []

  cells.each do |row|
    hash = Hash.new {''}
    row.each do |cell|
      cell.split('/').each_with_index do |r, i|
        hash[i] += r
      end
    end

    grid << hash.values.join('/')
  end
  grid.join('/')
end

# Takes a string of N lines, N characters each. Lines separated by /. Returns a
# larger grid with the same format.
# @param [String] grid
# @param [Hash] book
# @return [String]
def tick(grid, book)
  cells = (0 == grid.split('/').length % 2) ? split(grid, 2) : split(grid, 3)
  cells.map! {|row| row.map {|cell| enhance(cell, book)}}

  join(cells)
end

start = '.#./..#/###'
book = File.read('input').split("\n").map {|line| line.split(' => ')}.to_h

current = start
5.times {current = tick(current, book)}
puts current.split('').count {|c| '#' == c}
13.times {current = tick(current, book)}
puts current.split('').count {|c| '#' == c}

sleep 0.1
