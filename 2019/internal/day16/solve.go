package day16

import (
	"fmt"
	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

func generatePattern(length int, n_repeats int) []int {
	pattern := []int{}

	for len(pattern) < length+1 {
		for i := 0; i < n_repeats; i += 1 {
			pattern = append(pattern, 0)
		}
		for i := 0; i < n_repeats; i += 1 {
			pattern = append(pattern, 1)
		}
		for i := 0; i < n_repeats; i += 1 {
			pattern = append(pattern, 0)
		}
		for i := 0; i < n_repeats; i += 1 {
			pattern = append(pattern, -1)
		}
	}

	return pattern[1 : length+1]
}

func fft(numbers []int, n_phases int) []int {
	for phase := 0; phase < n_phases; phase += 1 {
		next := []int{}

		for nth := 0; nth < len(numbers); nth += 1 {
			// TODO: just calculate -1,0 or 1
			pattern := generatePattern(len(numbers), nth+1)

			sum := 0
			for i, number := range numbers {
				sum += number * pattern[i]
			}

			if sum < 0 {
				sum = -1 * sum
			}
			next = append(next, sum%10)
		}
		numbers = next
	}
	return numbers
}

func slice_to_int(slice []int) int {
	result := 0
	for _, n := range slice {
		result = result*10 + n
	}
	return result
}

func part1(numbers []int) int {
	numbers = fft(numbers, 100)

	return slice_to_int(numbers[:8])
}

func part2(numbers []int) int {
	offset := slice_to_int(numbers[:7])

	// TODO: just calculate
	repeated := []int{}
	for i := 0; i < 10_000; i += 1 {
		repeated = append(repeated, numbers...)
	}

	numbers = repeated[offset:]
	for phase := 0; phase < 100; phase += 1 {
		sum := 0
		for _, n := range numbers {
			sum += n
		}

		next := []int{}
		for i := 0; i < len(numbers); i += 1 {
			next = append(next, sum%10)
			sum -= numbers[i]
		}
		numbers = next
	}

	return slice_to_int(numbers[:8])
}

func Solve() error {
	line, err := utils.ReadSingleLine("day16/16.txt")
	if err != nil {
		return err
	}

	numbers := []int{}
	for _, r := range line {
		numbers = append(numbers, int(r-'0'))
	}

	fmt.Printf("Day 16, Part 1: %d\n", part1(numbers))
	fmt.Printf("Day 16, Part 2: %d\n", part2(numbers))

	return nil
}
