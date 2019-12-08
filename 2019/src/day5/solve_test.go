package day5

import (
	"testing"
	"utils"
)

const part1Answer = 13978427
const part2Answer = 11189491

func TestPart1(t *testing.T) {
	program, _ := utils.ReadProgram("5.txt")
	machine := utils.MakeMachine("day5")
	machine.LoadProgram(program)

	answer, _ := part1(machine)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, _ := utils.ReadProgram("5.txt")
	machine := utils.MakeMachine("day5")
	machine.LoadProgram(program)

	answer, _ := part2(machine)
	if part2Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part2Answer)
	}
}