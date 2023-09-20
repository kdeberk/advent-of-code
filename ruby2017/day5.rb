# 2017, Day 5.
#
# Calculate a path out of a maze of jump instructions. The jumps are represented by negative or positive
# numbers and they're either incremented or decremented after each jump. How many jumps does it take to leave
# the maze?
#
# Part 1: Increment all counters +1 after each jump.
# Part 2: Increment all counters +1 until the maximum of 3, then cycle between 2 and 3.
#
# There is a straightforward to implement part 2 in Ruby, but execution is quite slow ~2.5sec on an M1 Mac.
# so we have to come up with another approach. The input consists mostly of negative numbers

require 'byebug'

module Day5
  def self.NAME
    "Day 5: A Maze of Twisty Trampolines, All Alike"
  end

  def self.parse_input(input)
    input.split("\n").map(&:to_i)
  end

  def self.part1(input)
    jumps = input.clone

    idx = 0
    count = 0
    while idx < jumps.length
      nxt = idx + jumps[idx]
      jumps[idx] += 1
      idx = nxt
      count += 1
    end

    count
  end

  def self.part2(input)
    # Start at the end
    # For last, calculate number of visits needed to jump out
    # For last to one, calculate number of visits needed to jump out or

    byebug

    jumps = input.clone

    idx = 0
    count = 0
    while idx < jumps.length
      if 3 <= jumps[idx]
        jumps[idx] -= 1
        idx += jumps[idx] + 1
      else
        jumps[idx] += 1
        idx += jumps[idx] - 1
      end

      count += 1
    end

    count
  end
end
