package day3

import (
	"strconv"
	"strings"
)

type direction rune

const (
	up    direction = 'U'
	down  direction = 'D'
	left  direction = 'L'
	right direction = 'R'
)

type step struct {
	direction direction
	distance  int
}

type path []step

func readStep(string string) (step, error) {
	direction := direction(string[0])
	distance, err := strconv.ParseInt(string[1:], 10, 63)
	if err != nil {
		return step{}, err
	}
	return step{direction, int(distance)}, nil
}

func readPath(id int, line string) (path, error) {
	steps := []step{}

	for _, string := range strings.Split(line, ",") {
		a_step, err := readStep(string)

		if err != nil {
			return path{}, err
		}

		steps = append(steps, a_step)
	}
	return path(steps), nil
}
