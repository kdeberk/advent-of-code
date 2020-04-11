package day4

import "testing"

const part1Answer int = 2050
const part2Answer int = 1390

func TestPart1(t *testing.T) {
	answer := part1()
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	answer := part2()
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
