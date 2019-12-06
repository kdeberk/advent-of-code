package day2

import (
	"testing"
	"utils"
)

const part1Answer = 9581917
const part2Answer = 2505

func TestPart1(t *testing.T) {
	program, _ := utils.ReadProgram("2.txt")
	machine := utils.Machine{}
	machine.LoadProgram(program)

	answer, _ := part1(machine)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, _ := utils.ReadProgram("2.txt")
	machine := utils.Machine{}
	machine.LoadProgram(program)

	answer, _ := part2(machine)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
