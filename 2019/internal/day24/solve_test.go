package day24

import (
	"testing"
)

const part1Answer = 18842609
const part2Answer = 2059

func TestPart1(t *testing.T) {
	grid, err := readGrid("./../../input/24.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part1(grid)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	grid, err := readGrid("./../../input/24.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part2(grid)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
