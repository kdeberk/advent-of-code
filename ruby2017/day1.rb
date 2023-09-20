# 2017, Day 1
# Input forms a ring of digits. Loop through this ring and sum certain digits.
#
# Part 1: Sum the digits if the following digit is the same.
# Part 2: Sum the digits if the opposite-in-the-ring digit is the same.

module Day1
  def self.NAME
    "Day 1: Inverse Captcha"
  end

  def self.parse_input(input)
    input.strip
  end

  def self.sum_digits(input, skip)
    input.chars.each_with_index.map do |c, idx|
      if c == input[(idx+skip) % input.length]
        c.to_i
      else
        0
      end
    end.sum
  end

  def self.part1(input)
    sum_digits(input, 1)
  end

  def self.part2(input)
    sum_digits(input, input.length/2)
  end
end
