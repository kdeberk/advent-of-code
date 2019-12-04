package main

import (
	"fmt"
)

func part1_test(number uint64) bool {
	adjacents_matched := false

	prev := uint8(10)
	for 0 < number {
		current := uint8(number % 10)

		if prev < current {
			return false
		} else if current == prev {
			adjacents_matched = true
		}

		number /= 10
		prev = current
	}
	return adjacents_matched
}

func part2_test(number uint64) bool {
	counts := map[uint8]uint64{}

	prev := uint8(10)
	for 0 < number {
		current := uint8(number % 10)
		counts[current] += 1

		if prev < current {
			return false
		}

		number /= 10
		prev = current
	}

	for _, v := range counts {
		if 2 == v {
			return true
		}
	}
	return false
}

func count_matches(start uint64, end uint64, test func(n uint64) bool) uint64 {
	count := uint64(0)
	current := start
	for current <= end {
		if test(current) {
			count += 1
		}
		current += 1
	}
	return count
}

func main() {
	answer1 := count_matches(128392, 643281, part1_test)
	fmt.Printf("Part 1: %d\n", answer1)

	answer2 := count_matches(128392, 643281, part2_test)
	fmt.Printf("Part 2: %d\n", answer2)
}
