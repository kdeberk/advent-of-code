
def sum_of_divisors(n)
  sum = 0
  current = 1
  while current <= Math.sqrt(n)
    if 0 == n % current
      sum += current
      sum += n / current
    end
    current += 1
  end
  sum
end

puts sum_of_divisors(2 * 2 * 19 * 11 + (5 * 22 + 1))
puts sum_of_divisors(2 * 2 * 19 * 11 + (5 * 22 + 1) + (27 * 28 + 29) * 30 * 14 * 32)

