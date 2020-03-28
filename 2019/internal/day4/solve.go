package day4

import (
	"fmt"
)

func count_matches(start int, end int, test func(n int) bool) int {
	var count int

	for current := start; current <= end; current += 1 {
		if test(current) {
			count += 1
		}
	}
	return count
}

func part1() int {
	return count_matches(128392, 643281, func(number int) bool {
		adjacents_matched := false

		var prev int = 10
		var current int
		for 0 < number { // Iterating backwards over digits
			current, number = number%10, number/10

			if prev < current {
				// Digits are not in ascending order
				return false
			} else if current == prev {
				adjacents_matched = true
			}
			prev = current
		}
		return adjacents_matched
	})
}

func part2() int {
	return count_matches(128392, 643281, func(number int) bool {
		counts := map[int]int{}

		var prev int = 10
		var current int
		for 0 < number { // Iterating backwards over digits
			current, number = number%10, number/10

			if prev < current {
				// Digits are not in ascending order
				return false
			}
			counts[current] += 1
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
	fmt.Println("Day 4, Part 1. Count number of passwords that meet the criteria.")
	fmt.Println(" ", part1())
	fmt.Println("Day 4, Part 2. Count number of passwords that meet stricter criteria: a digit must appear exactly twice consecutively.")
	fmt.Println(" ", part2())
	return nil
}
