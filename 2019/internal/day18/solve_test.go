package day18

import (
	"testing"
)

const part1Answer = 4246
const part2Answer = 1940

func TestPart1(t *testing.T) {
	maze, err := readMaze("./../../input/18_part1.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := collectKeysInMaze(maze)
	if part1Answer != answer {
		t.Errorf("part1(input) == %d, want %d", answer, part1Answer)
	}
}

func TestPart2(t *testing.T) {
	maze, err := readMaze("./../../input/18_part2.txt")
	if err != nil {
		t.Fatal(err)
	}

	answer := collectKeysInMaze(maze)
	if part2Answer != answer {
		t.Errorf("part2(input) == %d, want %d", answer, part2Answer)
	}
}
