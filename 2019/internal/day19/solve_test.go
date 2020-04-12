package day19

import (
	"testing"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const part1Answer = 156
const part2Answer = 2610980

func TestPart1(t *testing.T) {
	program, err := utils.ReadProgram("./../../input/19.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part1(program)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, err := utils.ReadProgram("./../../input/19.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part2(program)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
