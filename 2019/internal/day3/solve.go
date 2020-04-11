package day3

import (
	"fmt"
)

func part1(grid grid) uint {
	origin, closest := point{0, 0}, point{1<<63 - 1, 1<<63 - 1}

	for _, intersection := range grid.getIntersections() {
		if origin.distance(intersection) < origin.distance(closest) {
			closest = intersection
		}
	}

	return origin.distance(closest)
}

func part2(grid grid) int {
	earliest := 1<<63 - 1

	for _, intersection := range grid.getIntersections() {
		sum := grid[intersection][0] + grid[intersection][1]

		if sum < earliest {
			earliest = sum
		}
	}

	return earliest
}

func Solve() error {
	grid, err := readGrid("./input/3.txt")
	if err != nil {
		return err
	}

	fmt.Println("Day 3, Part 1. Find the intersection of two wires that is closest to the origin (manhattan distance).")
	fmt.Println(" ", part1(grid))
	fmt.Println("Day 3, Part 2. Find the intersection of two wires that is closest to the origin (time travelled over wire)")
	fmt.Println(" ", part2(grid))
	return nil
}
