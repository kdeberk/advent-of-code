package day7

import (
	"testing"
	"utils"
)

const part1Answer = 46014
const part2Answer = 19581200

func TestPart1(t *testing.T) {
	program, _ := utils.ReadProgram("7.txt")

	answer, _ := part1(program)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, _ := utils.ReadProgram("7.txt")

	answer, _ := part2(program)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
