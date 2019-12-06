package day3

import "testing"

const part1Answer uint64 = 280
const part2Answer uint64 = 10554

func TestPart1(t *testing.T) {
	paths, _ := readPaths("3.txt")
	grid := Grid{}
	for _, path := range paths {
		grid.tracePath(path)
	}

	answer := part1(grid)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	paths, _ := readPaths("3.txt")
	grid := Grid{}
	for _, path := range paths {
		grid.tracePath(path)
	}

	answer := part2(grid)
	if part2Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part2Answer)
	}
}
