package day1

import (
	"testing"
	"utils"
)

const part1Answer = 3235550
const part2Answer = 4850462

func TestPart1(t *testing.T) {
	weights, _ := utils.ReadUint64s("1.txt", utils.IsNewline)

	answer := part1(weights)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	weights, _ := utils.ReadUint64s("1.txt", utils.IsNewline)

	answer := part2(weights)
	if part2Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part2Answer)
	}
}
