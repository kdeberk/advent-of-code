package day6

import (
	"testing"
)

const part1Answer = 251208
const part2Answer = 397

func TestPart1(t *testing.T) {
	tree, err := readTree("../../input/6.txt")
	if err != nil {
		t.Fatal("Could not open tree", err)
	}

	answer := part1(&tree)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	tree, err := readTree("../../input/6.txt")
	if err != nil {
		t.Fatal("Could not open tree", err)
	}

	answer := part2(&tree)
	if part2Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part2Answer)
	}
}
