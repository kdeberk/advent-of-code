package day4

import (
	"fmt"
)

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

func part1() uint64 {
	return count_matches(128392, 643281, func(number uint64) bool {
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
	})
}

func part2() uint64 {
	return count_matches(128392, 643281, func(number uint64) bool {
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
	})
}

func Solve() error {
	fmt.Printf("Day 4, Part 1: %d\n", part1())
	fmt.Printf("Day 4, Part 2: %d\n", part2())
	return nil
}
