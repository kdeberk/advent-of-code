package day23

import (
	"testing"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const part1Answer = 20367
const part2Answer = 15080

func TestPart1(t *testing.T) {
	program, err := utils.ReadProgram("./../../input/23.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer, _ := part1(program)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, err := utils.ReadProgram("./../../input/23.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer, _ := part2(program)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
