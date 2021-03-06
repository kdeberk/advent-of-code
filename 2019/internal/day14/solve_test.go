package day14

import "testing"

const part1Answer = 873899
const part2Answer = 1893569

func TestPart1(t *testing.T) {
	reactions, err := readReactions("./../../input/14.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part1(reactions)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	reactions, err := readReactions("./../../input/14.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part2(reactions)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
