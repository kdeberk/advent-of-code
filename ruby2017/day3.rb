# 2017, Day 3.
#
# Puzzle deal with a infinite grid of numbers that is generated in a spiral fashion.
#
# Part 1: Grid consists of rings where each ring has an odd length l, and the value of the
#  south east corner is l^2, and the others are l^2-l, l^2-2l and l^2-3l. Find the manhatten
#  distance between the location of the input on that grid, and the center of the grid.
# Part 2: Fill the grid from the inside out, until we reach the first number larger
#  than the input.

module Day3
  INPUT = 325489

  def self.NAME
    "Day 3: Spiral Memory"
  end

  def self.part1()
    l = Math.sqrt(INPUT).floor
    l += 1 if l.even?

    seCorner = l**2
    dRing   = l / 2                      # Distance from middle of side of ring to grid center.
    dMiddle = (seCorner - INPUT) % (l/2) # Distance from INPUT to middle of side of ring.
    dRing + dMiddle
  end

  def self.part2()
    h = Hash.new(0)
    h[[0, 0]] = 1

    ring, x, y = 1, 1, 0
    while true
      ring += 2

      x, y = fillSide(h, ring-1, x, y,  0, -1) # right face, upwards
      x, y = fillSide(h, ring,   x, y, -1,  0) # top face, leftwards
      x, y = fillSide(h, ring,   x, y,  0,  1) # left face, downwards
      x, y = fillSide(h, ring+1, x, y,  1,  0) # bottom face, rightwards.

      return h.values.select {|v| INPUT < v}.min if INPUT < h.values.max
    end
  end

  def self.fillSide(h, ring, x, y, dx, dy)
    (ring-1).times do
      h[[x, y]] =
          h[[x - 1, y - 1]] + h[[   x, y - 1]] + h[[x + 1, y - 1]] +
          h[[x - 1,     y]] +                  + h[[x + 1,     y]] +
          h[[x - 1, y + 1]] + h[[   x, y + 1]] + h[[x + 1, y + 1]]
      x, y = x+dx, y+dy
    end
    [x, y]
  end
end
