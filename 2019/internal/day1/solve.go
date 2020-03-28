package day1

import (
	"fmt"

	"github.com/kdeberk/advent-of-code/2019/internal/utils"
)

func calculateFuel(weight int) int {
	result := weight/3 - 2
	if 0 < result {
		return result
	} else {
		return 0
	}
}

func part1(weights []int) int {
	var fuelsum int

	for _, weight := range weights {
		fuelsum += calculateFuel(weight)
	}

	return fuelsum
}

func part2(weights []int) int {
	var fuelsum int

	for _, weight := range weights {
		for 0 < weight {
			fuel := calculateFuel(weight)
			fuelsum += fuel
			weight = fuel
		}
	}

	return fuelsum
}

func Solve() error {
	weights, err := utils.ReadInts("./input/1.txt", utils.IsNewline)
	if err != nil {
		return err
	}

	fmt.Println("Day 1, Part 1. Launch a spacecraft, calculate the fuel requirements for all components.")
	fmt.Println(" ", part1(weights))
	fmt.Println("Day 1, Part 2. Include the fuel itself as component for the fuel calculation.")
	fmt.Println(" ", part2(weights))
	return nil
}
