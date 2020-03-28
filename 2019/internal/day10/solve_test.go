package day10

import (
	"testing"
)

const part1Answer = 303
const part2Answer = 408

func TestPart1(t *testing.T) {
	asteroids, err := readAsteroids("../../input/10.txt")
	if err != nil {
		t.Fatal("Could not read asteroids", err)
	}

	answer := part1(asteroids)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	asteroids, err := readAsteroids("../../input/10.txt")
	if err != nil {
		t.Fatal("Could not read asteroids", err)
	}

	answer := part2(asteroids)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
