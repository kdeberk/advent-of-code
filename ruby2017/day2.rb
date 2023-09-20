# 2017, Day 2
# A grid of numbers consisting of multiple rows.
#
# Part 1: Sum the differences between and the smallest numbers per row.
# Part 2: Sum the ratios between the only two numbers that are each other's factor/multiple per row.

module Day2
  def self.NAME
    "Day 2: Corruption Checksum"
  end

  def self.parse_input(input)
    input.split("\n").map do |line|
      line.split(/\s+/).map(&:to_i).sort
    end
  end

  def self.part1(input)
    input.map {|row| row.last - row.first}.sum
  end

  def self.part2(input)
    input.map do |row|
      a, b = row.combination(2).find { |a, b| b % a == 0 }
      b / a
    end.sum
  end
end
