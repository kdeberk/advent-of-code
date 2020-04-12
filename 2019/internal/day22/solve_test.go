package day22

import (
	"testing"
)

const part1Answer = 6638
const part2Answer = 77863024474406

func TestPart1(t *testing.T) {
	steps, err := readSteps("./../../input/22.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part1(steps)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	steps, err := readSteps("./../../input/22.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part2(steps)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
