package day7

import (
	"testing"
	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const part1Answer = 46014
const part2Answer = 19581200

func TestPart1(t *testing.T) {
	program, err := utils.ReadProgram("../../input/7.txt")
	if err != nil {
		t.Fatal("Could not open program", err)
	}

	answer, _ := part1(program)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, err := utils.ReadProgram("../../input/7.txt")
	if err != nil {
		t.Fatal("Could not open program", err)
	}

	answer, _ := part2(program)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
