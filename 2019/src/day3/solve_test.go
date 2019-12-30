package day3

import "testing"

const part1Answer uint = 280
const part2Answer int = 10554

func TestPart1(t *testing.T) {
	grid, _ := readGrid("3.txt")

	answer := part1(grid)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	grid, _ := readGrid("3.txt")

	answer := part2(grid)
	if part2Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part2Answer)
	}
}
