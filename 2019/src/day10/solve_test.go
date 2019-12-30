package day10

import (
	"testing"
)

const part1Answer = 303
const part2Answer = 408

func TestPart1(t *testing.T) {
	asteroids, _ := readAsteroids("10.txt")

	answer := part1(asteroids)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	asteroids, _ := readAsteroids("10.txt")

	answer := part2(asteroids)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
