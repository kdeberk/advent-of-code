package day20

import (
	"testing"
)

const part1Answer = 510
const part2Answer = 5652

func TestPart1(t *testing.T) {
	maze, err := readMaze("./../../input/20.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part1(maze)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	maze, err := readMaze("./../../input/20.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := part2(maze)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
