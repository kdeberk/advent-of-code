package day12

import (
	"testing"
)

const part1Answer = 8960
const part2Answer = 314917503970904

var moons []moon = []moon{
	moon{vector{17, -12, 13}, vector{}},
	moon{vector{2, 1, 1}, vector{}},
	moon{vector{-1, -17, 7}, vector{}},
	moon{vector{12, -14, 18}, vector{}},
}

func TestPart1(t *testing.T) {
	answer := part1(moons)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	answer := part2(moons)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
