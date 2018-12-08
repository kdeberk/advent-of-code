#!/usr/bin/env ruby

LIMIT = 101

grid = LIMIT.times.map {[0] * LIMIT}
center = (LIMIT / 2.0).floor
grid[center][center] = 1

ring = 3
loop do
  d = (ring / 2.0).floor
  
  left = center - d
  right = center + d
  bottom = center - d
  top = center + d

  def calc_and_set_sum_of_neighbors(grid, x, y)
    grid[y][x] = 
        grid[y + 1][x - 1] + grid[y + 1][x] + grid[y + 1][x + 1] +
        grid[y    ][x - 1] +                  grid[y    ][x + 1] +
        grid[y - 1][x - 1] + grid[y - 1][x] + grid[y - 1][x + 1]
    
    if grid[y][x] > 325489
      puts grid[y][x]
      exit
    end
  end

  (ring - 1).times {|i| calc_and_set_sum_of_neighbors(grid, right, bottom + i + 1)}
  (ring - 1).times {|i| calc_and_set_sum_of_neighbors(grid, right - i - 1, top)}
  (ring - 1).times {|i| calc_and_set_sum_of_neighbors(grid, left, top - i - 1)}
  (ring - 1).times {|i| calc_and_set_sum_of_neighbors(grid, left + i + 1, bottom)}

  ring += 2
end

