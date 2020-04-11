package day2

import (
	"github.com/kdeberk/advent-of-code/2019/internal/utils"
	"testing"
)

const part1Answer = 9581917
const part2Answer = 2505

func TestPart1(t *testing.T) {
	program, err := utils.ReadProgram("../../input/2.txt")
	if err != nil {
		t.Fatal("Could not open program", err)
	}

	machine := utils.MakeMachine("day2", program)

	answer, _ := part1(machine)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, err := utils.ReadProgram("../../input/2.txt")
	if err != nil {
		t.Fatal("Could not open program", err)
	}

	machine := utils.MakeMachine("day2", program)

	answer, _ := part2(machine)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
