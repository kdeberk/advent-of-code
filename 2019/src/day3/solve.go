package day3

import (
	"fmt"
	"strconv"
	"strings"
	"utils"
)

type Point struct {
	x, y int64
}

func (self Point) distance(other Point) uint64 {
	return uint64(abs64(self.x-other.x) + abs64(self.y-other.y))
}

func abs64(x int64) int64 {
	if x < 0 {
		return -x
	} else {
		return x
	}
}

type Direction string

const (
	Up    Direction = "U"
	Down  Direction = "D"
	Left  Direction = "L"
	Right Direction = "R"
)

type Step struct {
	direction Direction
	distance  uint64
}

func readStep(string string) (Step, error) {
	direction := Direction(string[0])
	distance, err := strconv.ParseUint(string[1:], 10, 64)
	if err != nil {
		return Step{}, err
	}
	return Step{direction, distance}, nil
}

type Path struct {
	id    uint64
	steps []Step
}

func readPath(id uint64, line string) (Path, error) {
	steps := []Step{}

	for _, step := range strings.Split(line, ",") {
		step, err := readStep(step)

		if err != nil {
			return Path{}, err
		}

		steps = append(steps, step)
	}
	return Path{id, steps}, nil
}

func readPaths(filename string) ([]Path, error) {
	lines, err := utils.ReadStrings(filename, utils.IsNewline)
	if err != nil {
		return []Path{}, err
	}

	paths := []Path{}
	for index, line := range lines {
		path, err := readPath(uint64(index), line)
		if err != nil {
			return []Path{}, err
		}

		paths = append(paths, path)
	}

	return paths, nil
}

type Grid map[Point]map[uint64]uint64

func (self *Grid) tracePath(path Path) {
	time := uint64(1)
	current := Point{0, 0}

	for _, step := range path.steps {
		for i := uint64(0); i < step.distance; i++ {
			switch step.direction {
			case Up:
				current.y += 1
			case Down:
				current.y -= 1
			case Left:
				current.x -= 1
			case Right:
				current.x += 1
			}

			if _, ok := (*self)[current]; !ok {
				(*self)[current] = map[uint64]uint64{}
			}
			if _, ok := (*self)[current][path.id]; !ok {
				(*self)[current][path.id] = time
			}
			time += 1
		}
	}
}

func part1(grid Grid) uint64 {
	var origin Point = Point{0, 0}
	var closest Point = Point{1<<63 - 1, 1<<63 - 1}

	for point, v := range grid {
		if 2 == len(v) {
			if origin.distance(point) < origin.distance(closest) {
				closest = point
			}
		}
	}

	return origin.distance(closest)
}

func part2(grid Grid) uint64 {
	earliest := uint64(1<<64 - 1)
	for _, v := range grid {
		if 2 == len(v) {
			sum := uint64(0)
			for _, time := range v {
				sum += time
			}

			if sum < earliest {
				earliest = sum
			}
		}
	}

	return earliest
}

func Solve() error {
	paths, err := readPaths("day3/3.txt")
	if err != nil {
		return err
	}

	grid := Grid{}
	for _, path := range paths {
		grid.tracePath(path)
	}

	fmt.Printf("Day 3, Part 1: %d\n", part1(grid))
	fmt.Printf("Day 3, Part 2: %d\n", part2(grid))
	return nil
}