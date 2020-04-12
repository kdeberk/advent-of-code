package day15

import (
	"testing"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const part1Answer = 304
const part2Answer = 310

func TestPart1(t *testing.T) {
	program, err := utils.ReadProgram("./../../input/15.txt")
	if err != nil {
		t.Fatal(err)
	}

	remote := newRemoteControl(program)

	answer := part1(remote)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	program, err := utils.ReadProgram("./../../input/15.txt")
	if err != nil {
		t.Fatal(err)
	}

	remote := newRemoteControl(program)
	remote.explore()

	answer := part2(remote)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
