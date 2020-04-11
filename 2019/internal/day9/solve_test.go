package day9

import (
	"github.com/kdeberk/advent-of-code/2019/internal/utils"
	"testing"
)

const part1Answer = 3409270027
const part2Answer = 82760

func TestPart1(t *testing.T) {
	program, err := utils.ReadProgram("../../input/9.txt")
	if err != nil {
		t.Fatal("Could not open program", err)
	}

	answer, _ := part1(program)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, err := utils.ReadProgram("../../input/9.txt")
	if err != nil {
		t.Fatal("Could not open program", err)
	}

	answer, _ := part2(program)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
