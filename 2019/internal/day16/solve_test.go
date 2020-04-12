package day16

import (
	"testing"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

const part1Answer = 52611030
const part2Answer = 52541026

func TestPart1(t *testing.T) {
	digits, err := utils.ReadDigits("./../../input/16.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part1(digits)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	digits, err := utils.ReadDigits("./../../input/16.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part2(digits)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
