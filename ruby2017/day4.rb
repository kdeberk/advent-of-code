# 2017, Day 4.
#
# Input consists of passphrases, one per line, and each passphrase consists of random words.
# Goal is to count how many passphrases are valid
#
# Part 1: Valid passphrase do not have duplicate words.
# Part 2: Valid passphrase do not contain two words that are anagrams of each other.

require 'set'

module Day4
  def self.NAME
    "Day 4: High-Entropy Passphrases"
  end

  def self.parse_input(input)
    input.lines.map {|line| line.split(' ')}
  end

  def self.all_unique?(words)
    words.length == Set.new(words).length
  end

  def self.part1(input)
    input.count {|words| all_unique? words}
  end

  def self.part2(input)
    input.count {|words| all_unique? words.map {|word| word.chars.sort}}
  end
end
