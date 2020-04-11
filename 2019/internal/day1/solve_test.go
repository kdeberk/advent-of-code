package day1

import (
	"testing"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const part1Answer = 3235550
const part2Answer = 4850462

func TestPart1(t *testing.T) {
	weights, err := utils.ReadInts("../../input/1.txt", utils.IsNewline)
	if err != nil {
		t.Fatal("Could not read ints", err)
	}

	answer := part1(weights)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	weights, err := utils.ReadInts("../../input/1.txt", utils.IsNewline)
	if err != nil {
		t.Fatal("Could not read ints", err)
	}

	answer := part2(weights)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
