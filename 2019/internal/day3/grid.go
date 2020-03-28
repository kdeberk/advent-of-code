package day3

import "github.com/kdeberk/advent-of-code/2019/internal/utils"

type grid map[point]map[int]int

func readGrid(filename string) (grid, error) {
	lines, err := utils.ReadStrings(filename, utils.IsNewline)
	if err != nil {
		return grid{}, err
	}

	grid := grid{}
	for index, line := range lines {
		path, err := readPath(int(index), line)

		if err != nil {
			return grid, err
		}

		grid.tracePath(index, path)
	}

	return grid, nil
}

func (self *grid) tracePath(index int, path path) {
	time := 1
	current := point{0, 0}

	for _, a_step := range path {
		for i := 0; i < a_step.distance; i++ {
			current.x += map[direction]int{up: -1, down: 1, left: 0, right: 0}[a_step.direction]
			current.y += map[direction]int{up: 0, down: 0, left: -1, right: 1}[a_step.direction]

			if _, ok := (*self)[current]; !ok {
				(*self)[current] = map[int]int{}
			}
			if _, ok := (*self)[current][index]; !ok {
				(*self)[current][index] = time
			}
			time += 1
		}
	}
}

func (self *grid) getIntersections() []point {
	intersections := []point{}

	for point, v := range *self {
		if 1 < len(v) {
			intersections = append(intersections, point)
		}
	}

	return intersections
}
