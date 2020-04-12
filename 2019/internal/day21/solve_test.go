package day21

import (
	"testing"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const part1Answer = 19358870
const part2Answer = 1143356492

func TestPart1(t *testing.T) {
	program, err := utils.ReadProgram("./../../input/21.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer, _ := part1(program)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, err := utils.ReadProgram("./../../input/21.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer, _ := part2(program)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
