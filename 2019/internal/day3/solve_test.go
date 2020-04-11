package day3

import "testing"

const part1Answer uint = 280
const part2Answer int = 10554

func TestPart1(t *testing.T) {
	grid, err := readGrid("../../input/3.txt")
	if err != nil {
		t.Fatal("Could not open grid", err)
	}

	answer := part1(grid)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	grid, err := readGrid("../../input/3.txt")
	if err != nil {
		t.Fatal("Could not open grid", err)
	}

	answer := part2(grid)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
